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
stateTransition _ _ ioChan = return $ Left "Not implemented 6"



-- for testing
main :: IO ()
main = do
    -- Test for LoadCommand
    print $ parseCommand "LOAD"          -- Expected: Right (LoadCommand, "")
    
    -- Test for SaveCommand
    print $ parseCommand "SAVE"          -- Expected: Right (SaveCommand, "")
    
    -- Test for a single ADD command
    print $ parseCommand "ADD dog Max 5" 
    -- Right (StatementCommand (Single (Add (Animal {species = "dog", name = "Max", age = 5}))),"")
    
    -- Test for a single DELETE command
    print $ parseCommand "DELETE cat Whiskers 3" 
    -- Right (StatementCommand (Single (Delete (Animal {species = "cat", name = "Whiskers", age = 3}))),"")
    
    -- Test for a batch of commands (ADD + DELETE)
    print $ parseCommand "ADD dog Max 5; DELETE cat Whiskers 3"
    -- Right (StatementCommand (Batch [Add (Animal {species = "dog", name = "Max", age = 5}),Delete (Animal {species = "cat", name = "Whiskers", age = 3})]),"")
    
    -- Test for invalid command
    print $ parseCommand "INVALID_COMMAND"
    -- Left "One or more queries in the batch could not be parsed."