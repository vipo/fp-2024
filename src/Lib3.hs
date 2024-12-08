{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Parser(..),
    Statements(..),
    Command(..)
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (forever)

import qualified Lib2
import Parser

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = forever $ do
    op <- readChan chan
    case op of
        Save content respondChan -> do
            writeFile "state.txt" content
            writeChan respondChan ()
        Load respondChan -> do
            content <- readFile "state.txt"
            writeChan respondChan content

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = showQuery q
  show (Batch qs) = "START\n" ++ concatMap ((++ ";\n") . showQuery) qs ++ "FINISH\n"

showQuery :: Lib2.Query -> String
showQuery Lib2.ViewDeck = "view"
showQuery (Lib2.AddDeck deck) = "add " ++ show deck
showQuery Lib2.DeleteDeck = "delete"



data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = runParser command

command :: Parser Command
command =
  (do
    StatementCommand <$> parseStatements
  )
  <|> parseSave
  <|> parseLoad

parseSave :: Parser Command
parseSave = do
  _ <- parseString "save"
  return SaveCommand

parseLoad :: Parser Command
parseLoad = do
  _ <- parseString "load"
  return LoadCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: Parser Statements
parseStatements =
  (do
    _ <- parseString "START\n"
    qs <- many (do
                  q <- Lib2.parseQuery'
                  _ <- parseString ";\n"
                  return q
                )
    _ <- parseString "FINISH\n"
    return (Batch qs)
  )
  <|> (do
    Single <$> Lib2.parseQuery'
  )


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State maybeDeck) =
  case maybeDeck of
    Nothing -> Single Lib2.DeleteDeck
    Just deck -> Batch [Lib2.DeleteDeck, Lib2.AddDeck deck]



-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements = show

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
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar SaveCommand ioChan = do
  current <- readTVarIO stateVar
  resultChan <- newChan
  writeChan ioChan (Save (renderStatements $ marshallState current) resultChan)
  _ <- readChan resultChan
  return $ Right $ Just "State saved."

stateTransition stateVar LoadCommand ioChan = do
  resultChan <- newChan
  writeChan ioChan (Load resultChan)
  dataString <- readChan resultChan
  case runParser parseStatements dataString of
    Left parseErr -> return $ Left $ "Load failed:\n" ++ parseErr
    Right (parsedCmds, _) -> stateTransition stateVar (StatementCommand parsedCmds) ioChan

stateTransition stateVar (StatementCommand cmds) _ = atomically $ processStatements stateVar cmds

processQueries :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
processQueries state [] = Left "No queries to process"
processQueries state (q:qs) = case Lib2.stateTransition state q of
  Left err -> Left err
  Right (msg, newState) ->
    if null qs
      then Right (msg, newState)
      else case processQueries newState qs of
        Left err' -> Left err'
        Right (msg', finalState) ->
          let combinedMsg = (++) <$> fmap (++ "\n") msg <*> msg'
          in Right (combinedMsg, finalState)

processStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
processStatements stateVar (Batch cmds) = do
  currentState <- readTVar stateVar
  case processQueries currentState cmds of
    Left err -> return $ Left err
    Right (msg, updatedState) -> do
      writeTVar stateVar updatedState
      return $ Right msg

processStatements stateVar (Single cmd) = do
  currentState <- readTVar stateVar
  case Lib2.stateTransition currentState cmd of
    Left err -> return $ Left err
    Right (msg, updatedState) -> do
      writeTVar stateVar updatedState
      return $ Right msg



