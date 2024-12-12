module Main where

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Control.Concurrent.STM (newTVarIO, TVar, atomically, readTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, Chan)
import Web.Scotty
import Lib3
import qualified Lib2 -- Ensure Lib2 is imported for State

main :: IO ()
main = do
    stateVar <- newTVarIO Lib2.emptyState -- Shared state
    ioChan <- newChan
    _ <- forkIO $ storageOpLoop ioChan -- Start storage operation loop

    scotty 3000 $ do
        post (literal "/") $ do
            bodyText <- body
            response <- liftIO $ processBatch stateVar ioChan (cs bodyText)
            text $ cs response

-- Process a batch of commands
processBatch :: TVar Lib2.State -> Chan StorageOp -> String -> IO String
processBatch stateVar ioChan input = case parseCommand input of
    Left err -> return $ "Error parsing input: " ++ err
    Right (command, _) -> do
        result <- stateTransition stateVar command ioChan
        return $ case result of
            Left err -> "Error: " ++ err
            Right (msg, _) -> maybe "Success" id msg
