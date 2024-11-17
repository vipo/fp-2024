{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Concurrent.Chan
import Control.Concurrent.STM(TVar, newTVarIO)
import Control.Monad.IO.Class ()
import Control.Monad.State.Strict
    ( MonadIO(liftIO), evalStateT, StateT, get, lift)
import Data.List qualified as L
import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    MultiLine (..),
    evalRepl,
  )
import Control.Concurrent (forkIO)

type Repl a = HaskelineT (StateT (TVar Lib2.State, Chan Lib3.StorageOp) IO) a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Press [TAB] for auto completion, type ':paste' for multiline input"

completer :: (Monad m) => WordCompleter m
completer n =
  return $ ":paste" : Prelude.filter (L.isPrefixOf n) Lib1.completions

cmd :: String -> Repl ()
cmd str = do
  case Lib3.parseCommand str of
    Left e -> liftIO $ putStrLn $ "PARSE ERROR:" ++ e
    Right (c, "") -> do
      (st, chan) <- lift get
      tr <- liftIO $ Lib3.stateTransition st c chan
      case tr of
        Left e2 -> liftIO $ putStrLn $ "ERROR:" ++ e2
        Right m -> mapM_ (liftIO . putStrLn) m
    Right (_, r) -> liftIO $ putStrLn $ "PARSE ERROR: string is not fully consumed - " ++ r

invite :: MultiLine -> Repl String
invite SingleLine = pure ">>> "
invite MultiLine = pure "| "

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  evalStateT (evalRepl invite cmd [] (Just ':') (Just "paste") (Word completer) ini final)
    (state, chan) 
