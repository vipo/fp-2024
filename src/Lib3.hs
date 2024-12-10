{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where


import Control.Concurrent ( Chan )
import qualified Lib2
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar, readTVarIO, modifyTVar)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop _ = do
  return $ error "Not implemented 1"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand _ = Left "Not implemented 2"

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements _ = Left "Not implemented 3"

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState _ = error "Not implemented 4"

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements _ = error "Not implemented 5"

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
stateTransition state command ioChan =
  case command of
    StatementCommand sc ->
      updateState state sc
    SaveCommand -> do
      jakie <- newChan
      st <- readTVarIO state
      writeChan ioChan (Save (renderStatements (marshallState st)) jakie)
      _ <- readChan jakie
      return $ Right Nothing
    LoadCommand -> do
      jakie <- newChan
      writeChan ioChan (Load jakie)
      file <- readChan jakie
      case parseStatements file of
        Left e -> return $ Left e
        Right (st, "") -> updateState state st
        Right _ -> return $ Left "Statements are not fully parsed"

updateState :: TVar Lib2.State -> Statements ->
               IO (Either String (Maybe String))
updateState state (Single q) = atomically $ updateState' state [q]
updateState state (Batch b) = atomically $ updateState' state b

updateState' :: TVar Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String))
updateState' _ [] = return $ Right Nothing
updateState' state (h:t) = do
  case h of
    Lib2.Print -> do
      Lib2.Sum s <- readTVar state
      us <- updateState' state t
      case us of
        Left e -> return $ Left e
        Right Nothing -> return $ Right $ Just $ show s
        Right (Just next) -> return $ Right $ Just $ concat [show s, "\n", next]
    Lib2.Add v -> do
      modifyTVar state (\(Lib2.Sum s) -> Lib2.Sum (s + v))
      return $ Right Nothing