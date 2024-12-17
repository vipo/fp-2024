{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Lib2
import qualified Lib3
import Web.Scotty

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  scotty 3000 $
    post "/" $ do
      b <- body
      liftIO $ putStrLn ("Request was: " ++ cs b)
      response <- liftIO $ proccess state chan $ cs b
      text $ cs response

proccess :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
proccess state storageChan input = case Lib3.parseCommand input of
  Left e -> return e
  Right (cmd, "") -> do
    info <- Lib3.stateTransition state cmd storageChan
    case info of
      Left e -> return e
      Right mb -> return $ fromMaybe "Success" mb
  Right (_, str) -> return $ "Could not parse: " ++ str