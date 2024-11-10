{-# LANGUAGE InstanceSigs #-}

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
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Exception (try, SomeException)
import Control.Concurrent.STM (STM, TVar)
import Data.Either (partitionEithers)
import Control.Concurrent (Chan)
import Data.List (isPrefixOf)
import qualified Lib2


data StorageOp = Save String (Chan ()) | Load (Chan String)

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


-- DT that represents batch or single query (Add Animal, Delete Animal, ListAnimals, CompoundQuery Query Query)
-- 'Statements' gives only 'Single' or 'Batch'
data Statements = Batch [Lib2.Query] | Single Lib2.Query
    deriving (Show, Eq)

data Command = StatementCommand Statements | LoadCommand | SaveCommand
    deriving (Show, Eq)



parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "LOAD" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
  | "SAVE" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | otherwise = -- if input does not start w "LOAD" or "SAVE"
      case parseStatements input of
          Left err -> Left err
          Right (gotStatements, rest) -> Right (StatementCommand gotStatements, rest)

-- | Parses Statement(s) from the input.
-- Decides if it's a single query or a batch of queries.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
    let queries = map (Lib2.parseQuery . trim) (splitOn ';' input) -- breaks down the input into individual commands and attempts to parse each one
        (errors, parsedQueries) = partitionEithers queries
    in case errors of
         [] -> if length parsedQueries == 1
                 then Right (Single (head parsedQueries), "")
                 else Right (Batch parsedQueries, "")
         _ -> Left "One or more queries in the batch could not be parsed."


-- to trim whitespace
trim :: String -> String
trim = f . f -- applies f and applies f again on result
   where f = reverse . dropWhile (== ' ') -- drops ' ', reverses, drops ' ', reverses again

-- to split input
splitOn :: Char -> String -> [String] -- Char - the delimiter
splitOn _ [] = [""] -- base case
splitOn delimiter (c:cs) -- check if the current character c is the delimiter
    | c == delimiter = "" : rest -- when c is the delimiter, we start a new segment by adding an empty string at the beginning of rest
    | otherwise = (c : head rest) : tail rest -- when c is not the delimiter, we add it to the start of the first segment in rest
  where
    rest = splitOn delimiter cs -- rest - the result of applying splitOn to the remaining characters cs





-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
-- translates the current State into a minimal list of operations (represented as Statements) 
-- that could recreate the current state if applied to an empty state
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State animals) =
    let addQueries = map Lib2.Add animals  -- Create an `Add` query for each animal
    in Batch addQueries  -- Wrap the list of `Add` queries in a `Batch`


-- do i need? other way???
-- to load the State from a parsed representation
unmarshallState :: Statements -> LibState
unmarshallState = undefined  -- Define this as needed

-- takes the Statements (produced by marshallState) and converts it
-- back into a String that follows the BNF syntax, allowing it to be 
-- saved and later parsed back into Statements.

-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")

renderStatements :: Statements -> String -- Renders Statements into a String which can be parsed back into Statements
renderStatements (Single query) = renderQuery query  -- Render a single query directly
renderStatements (Batch queries) = unwords ["BEGIN", renderBatch queries, "END"]  -- Wrap batch in BEGIN and END

-- renders a batch of queries as a ; separeted string
renderBatch :: [Lib2.Query] -> String
renderBatch [] = ""
renderBatch (q:qs) = renderQuery q ++ "; " ++ renderBatch qs
-- here: q - first query, gs - remaining queries

-- to convert individual Queries to original string
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.Add (Lib2.Animal species name age)) =
    "ADD " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery (Lib2.Delete (Lib2.Animal species name age)) =
    "DELETE " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery Lib2.ListAnimals = 
    "LIST"
renderQuery (Lib2.CompoundQuery q1 q2) =  -- recursively i could add here as many quaries as i want
    renderQuery q1 ++ "; " ++ renderQuery q2

-- needs modifying
-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, Lib2.State))
stateTransition stateVar command ioChan = case command of
    LoadCommand -> do -- load state from a file
        responseChan <- newChan  -- create a new channel to receive the loaded data
        writeChan ioChan (Load responseChan)  -- send Load request to storageOpLoop
        result <- readChan responseChan  -- waits for response
        case parseStatements result of  -- parses the loaded data
            Left err -> return $ Left ("Error loading state: " ++ err)
            Right (loadedStatements, _) -> do
                let loadedState = unmarshallState loadedStatements  -- parsed data to state
                atomically $ writeTVar stateVar loadedState  -- updates the state atomically
                return $ Right (Just "State loaded from file.", loadedState)

    -- save current state to a file
    SaveCommand -> do
        currentState <- atomically $ readTVar stateVar  -- read the current state
        let statements = marshallState currentState  -- marshallState converts state to minimal representation
        let serializedState = renderStatements statements  -- to string
        responseChan <- newChan  -- new channel to receive the save confirmation
        writeChan ioChan (Save serializedState responseChan)  -- aend Save request
        _ <- readChan responseChan  -- wait for confirmation (no result expected)
        return $ Right (Just "State saved to file.", currentState)

    -- execute a single query or batch of queries
    StatementCommand statements -> do
        finalResult <- atomically $ processStatements stateVar statements  -- Process batch or single query atomically
        return finalResult
  where
    -- to process Statements (Single/Batch) atomically
    processStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String, Lib2.State))
    processStatements tvar (Single query) = executeQuery tvar query
    processStatements tvar (Batch queries) = do
        initialState <- readTVar tvar
        processBatch initialState queries
      where
        processBatch :: Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String, Lib2.State))
        processBatch currentState [] = return $ Right (Just "Batch processed.", currentState)
        processBatch currentState (q:qs) = do
            result <- executeQuerySTM currentState q
            case result of
                Left err -> return $ Left err
                Right (msg, newState) -> processBatch newState qs

    -- Execute individual query and update TVar state
    executeQuerySTM :: Lib2.State -> Lib2.Query -> STM (Either String (Maybe String, Lib2.State))
    executeQuerySTM




