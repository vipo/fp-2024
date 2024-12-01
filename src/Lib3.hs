{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TVar
import Data.Maybe (fromMaybe)
import Control.Concurrent ( Chan, readChan, writeChan )
import Lib2 (parseLiteral)
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content replychan -> do 
      writeFile "state.txt" content
      writeChan replychan ()
    Load responseChan -> do 
      content <- readFile "state.txt"
      writeChan responseChan content 
  storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

parseSave :: String -> Either String (Command, String)
parseSave input = 
  case parseLiteral "save" (Lib2.strip input) of
    Right (_, rest) -> Right (LoadCommand, rest)
    Left _ -> Left ""


parseLoad :: String -> Either String (Command, String)
parseLoad input =
  case parseLiteral "load" (Lib2.strip input) of
    Right (_, rest) -> Right (SaveCommand, rest)
    Left _ -> Left ""

parseStatementsCommand :: String -> Either String (Command, String)
parseStatementsCommand input = 
  case parseStatements input of
    Right (statements, rest) -> Right (StatementCommand statements, rest)
    Left err -> Left $ "Failed to parse command\n" ++ err

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand =
  Lib2.or3'
    parseLoad
    parseSave
    parseStatementsCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = do
  case Lib2.parseQuery input of
    Left err -> Left err
    Right (query, rest) ->
      if null rest
        then Right (Single query, rest)
        else case parseStatements rest of 
          Left err -> Left err
          Right (Batch qs, remaining) -> Right (Batch (query : qs), remaining) 


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = 
  Batch (map Lib2.AddArtwork (Lib2.artworks state))

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderStatements (Batch [query])
renderStatements (Batch queries) = unlines $ map getStringFromQuery queries


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- Batches must be executed atomically (STM).
-- Right contains an optional message to print and
-- an updated program's state (potentially loaded from a file)
stateTransition :: 
  TVar Lib2.State -> 
  Command -> 
  Chan StorageOp ->
  IO (Either String (Maybe String, Lib2.State))
stateTransition st command ioChan = case command of
  LoadCommand -> do
    replyChan <- newChan
    writeChan ioChan (Load replyChan)
    content <- readChan replyChan
    case parseStatements content of
      Left err -> return $ Left err
      Right (statements, _) -> atomically $ do
        let (_, newState) = applyQueries Lib2.emptyState (getQueries statements)
        writeTVar st newState
        return $ Right (Just "State loaded successfully.", newState)
  
  SaveCommand -> do
    currentState <- readTVarIO st
    result <- saveStateToFile currentState ioChan
    return $ case result of
      Left err -> Left err
      Right () -> Right (Just "State saved successfully.", currentState)
  
  StatementCommand statements -> atomically $ do
    currentState <- readTVar st
    let (msg, newState) = applyQueries currentState (getQueries statements)
    writeTVar st newState
    return $ Right (Just ("Commands parsed\nLog:\n" ++ msg), newState)

applyQueries :: Lib2.State -> [Lib2.Query] -> (String, Lib2.State)
applyQueries st [] = ("", st)
applyQueries st (q : qs) =
  let (msg, newState) = applyQuery st q
      (otherMsg, finalState) = applyQueries newState qs
   in (msg ++ "\n" ++ otherMsg, finalState)

getQueries :: Statements -> [Lib2.Query]
getQueries (Single query) = [query]
getQueries (Batch queries) = queries

saveStateToFile :: Lib2.State -> Chan StorageOp -> IO (Either String ())
saveStateToFile state ioChan = do
  let serializedState = renderStatements (marshallState state)
  replyChan <- newChan
  writeChan ioChan (Save serializedState replyChan)
  -- Wait for the operation to complete
  result <- readChan replyChan
  return $ Right result

getStringFromQuery :: Lib2.Query -> String
getStringFromQuery query = case query of
  Lib2.AddArtwork artPiece ->
    "add_artwork(" ++
    show (Lib2.artId artPiece) ++ " " ++
    show (Lib2.title artPiece) ++ " " ++
    show (Lib2.artType artPiece) ++ " " ++
    show (Lib2.price artPiece) ++ " " ++
    show (Lib2.description artPiece) ++ ")"
  Lib2.SellArtwork artPiece ->
    "sell_artwork(" ++ show (Lib2.artId artPiece) ++ ")"
  Lib2.PrintInfo ->
    "print_info()"
  Lib2.Sequence queries ->
    concatMap (\q -> getStringFromQuery q ++ "; ") queries
