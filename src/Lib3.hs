{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib3
    ( stateTransition,
      StorageOp (..),
      storageOpLoop,
      parseCommand,
      parseStatements,
      marshallState,
      renderStatements,
      Statements(..),
      Command(..),
    ) where

import System.IO (withFile, IOMode(..), hPutStr, hGetContents)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (try, SomeException)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar, readTVarIO)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Lib2
import Debug.Trace (trace)

-- Define the storage operations (saving and loading)
data StorageOp = Save String (Chan ()) | Load (Chan String)

-- Background thread function that handles storage operations
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content notifyChan -> do
      result <- try $ withFile "state.txt" WriteMode $ \handle -> hPutStr handle content
      case result of
        Left err -> putStrLn $ "Error saving state: " ++ show (err :: SomeException)
        Right _  -> writeChan notifyChan ()
      storageOpLoop chan

    Load notifyChan -> do
      result <- try $ withFile "state.txt" ReadMode hGetContents
      case result of
        Left err     -> writeChan notifyChan $ "Error loading state: " ++ show (err :: SomeException)
        Right content -> writeChan notifyChan content
      storageOpLoop chan

-- Data structure to represent single or batch statements
data Statements = Batch [Lib2.Query] | Single Lib2.Query
    deriving (Show, Eq)

data Command = StatementCommand Statements | LoadCommand | SaveCommand
    deriving (Show, Eq)

-- Parses the command
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "LOAD" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
  | "SAVE" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | "BEGIN" `isPrefixOf` input = case parseStatements input of
      Left err -> Left err
      Right (gotStatements, rest) -> Right (StatementCommand gotStatements, rest)
  | otherwise = Left "Expected 'LOAD', 'SAVE', or a batch starting with 'BEGIN'."

-- Parses statements (single or batch)
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let inputTrimmed = trim input
  in if "BEGIN" `isPrefixOf` inputTrimmed
       then
         if "END" `isSuffixOf` inputTrimmed
           then
             let body = trim $ drop 5 $ take (length inputTrimmed - 3) inputTrimmed
             in if null body
                  then Right (Batch [], "") -- Allow an empty batch
                  else
                    let queries = map (Lib2.parseQuery . trim) (filter (not . null) $ splitOn ';' body)
                        (errors, parsedQueries) = partitionEithers queries
                    in if null errors
                         then Right (Batch parsedQueries, "")
                         else Left $ "Error parsing queries: " ++ show errors
           else Left "Expected 'END' for batch processing."
       else Left "Expected 'BEGIN' and 'END' for batch processing."

-- Utility functions for trimming and splitting strings
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (`elem` [' ', '\n', '\r', '\t'])

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delimiter cs

-- Convert the program's state to statements (to save it in a file)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State animals) =
    let addQueries = map Lib2.Add animals
    in Batch addQueries

-- Load the state from statements
unmarshallState :: Statements -> Lib2.State
unmarshallState (Batch queries) = foldl applyQuery Lib2.emptyState queries
  where
    applyQuery state query = case Lib2.stateTransition state query of
      Right (_, newState) -> newState
      Left _ -> state -- Ignore errors for simplicity

-- Convert statements to string representation
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = "BEGIN\n" ++ renderBatch queries ++ "END\n"

renderBatch :: [Lib2.Query] -> String
renderBatch [] = ""
renderBatch (q:qs) = renderQuery q ++ ";\n" ++ renderBatch qs

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.Add (Lib2.Animal species name age)) = "ADD " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery (Lib2.Delete (Lib2.Animal species name age)) = "DELETE " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery Lib2.ListAnimals = "LIST"
renderQuery (Lib2.CompoundQuery q1 q2) = renderQuery q1 ++ "; " ++ renderQuery q2

-- Update state based on a command
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, Lib2.State))
stateTransition stateVar command ioChan = case command of
    LoadCommand -> do
        responseChan <- newChan
        writeChan ioChan (Load responseChan)
        result <- readChan responseChan
        case parseStatements result of
            Left err -> return $ Left ("Error loading state: " ++ err)
            Right (loadedStatements, _) -> do
                let loadedState = unmarshallState loadedStatements
                atomically $ writeTVar stateVar loadedState
                return $ Right (Just "State loaded from file.", loadedState)

    SaveCommand -> do
        currentState <- readTVarIO stateVar
        let statements = marshallState currentState
        let serializedState = renderStatements statements
        responseChan <- newChan
        writeChan ioChan (Save serializedState responseChan)
        _ <- readChan responseChan
        return $ Right (Just "State saved to file.", currentState)

    StatementCommand statements -> do
        atomically $ processStatements stateVar statements
  where
    processStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String, Lib2.State))
    processStatements tvar (Single query) = executeQuerySTM tvar query
    processStatements tvar (Batch queries) = do
        initialState <- readTVar tvar
        processBatch initialState queries
      where
        processBatch :: Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String, Lib2.State))
        processBatch currentState [] = return $ Right (Just "Batch processed.", currentState)
        processBatch currentState (q:qs) = do
            result <- executeQuerySTM tvar q
            case result of
                Left err -> return $ Left err
                Right (msg, newState) -> do
                    rest <- processBatch newState qs
                    case rest of
                        Left err -> return $ Left err
                        Right (restMsg, finalState) ->
                            let combinedMsg = unwords $ filter (not . null) [maybe "" id msg, maybe "" id restMsg]
                            in return $ Right (Just combinedMsg, finalState)

    executeQuerySTM :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String, Lib2.State))
    executeQuerySTM stateVar query = do
        currentState <- readTVar stateVar
        case Lib2.stateTransition currentState query of
            Left err -> return $ Left err
            Right (msg, newState) -> do
                writeTVar stateVar newState
                return $ Right (msg, newState)
