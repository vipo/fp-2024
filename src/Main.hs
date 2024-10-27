module Main where

import Lib2 (parseQuery, stateTransition, emptyState, State)

main :: IO ()
main = replLoop emptyState  -- corrected to use replLoop

replLoop :: State -> IO ()
replLoop state = do
  putStrLn "Enter command (or type `exit` to quit):"
  input <- getLine
  if input == "exit"
    then putStrLn "Goodbye!"
    else do
      let result = parseQuery input >>= stateTransition state
      case result of
        Left err -> putStrLn ("Error: " ++ err) >> replLoop state
        Right (Just message, newState) -> putStrLn message >> replLoop newState
        Right (Nothing, newState) -> replLoop newState
