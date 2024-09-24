{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class ()
import Control.Monad.State.Strict
import Data.List qualified as L
import Lib1 qualified
import Lib2 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

type Repl a = HaskelineT (StateT Lib2.State IO) a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer n =
  return $ Prelude.filter (L.isPrefixOf n) Lib1.completions

cmd :: String -> Repl ()
cmd str = do
  case Lib2.parseQuery str of
    Left e -> liftIO $ putStrLn $ "PARSE ERROR:" ++ e
    Right e -> do
      st <- lift get
      case Lib2.stateTransition st e of
        Left e2 -> liftIO $ putStrLn $ "ERROR:" ++ e2
        Right (m, ns) -> lift (put ns) >> mapM_ (liftIO . putStrLn) m

main :: IO ()
main = evalStateT
  (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) Lib2.emptyState
