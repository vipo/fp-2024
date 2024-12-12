{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib3
    ( stateTransition,
      StorageOp (..),
      storageOpLoop,
      parseCommand,
      parseStatements,
      marshallState,
      unmarshallState,
      renderStatements,
      processCommand,
      Statements(..),
      Command(..),
    ) where

import qualified Lib2
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Exception (try, SomeException)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)



-- DT for storage opperations
-- Chan - a notification/communication channel - nofifies the caller when the operation completes
-- / or throught it result of loading op will be sent back to the caller
data StorageOp = Save String (Chan ()) | Load (Chan String)



-- Background thread for storage operations
storageOpLoop :: Chan StorageOp -> IO () -- takes Save | Load
storageOpLoop chan = do
  op <- readChan chan -- waits for a StorageOp val
  case op of

    Save content notifyChan -> do
      result <- try $ writeFile "state.txt" content
      case result of
        Left err -> putStrLn $ "Error saving state: " ++ show (err :: SomeException) -- if err, msg to the console
        Right _  -> writeChan notifyChan () -- if success, notifyChan sends a signal (()) back to the caller
      storageOpLoop chan -- calls itself - ready to handle more operations

    Load notifyChan -> do
      result <- try $ readFile "state.txt"
      case result of
        Left err      -> writeChan notifyChan $ "Error loading state: " ++ show (err :: SomeException)
        Right content -> writeChan notifyChan content
      storageOpLoop chan



-- DT
data Statements = Batch [Lib2.Query] | Single Lib2.Query
    deriving (Show, Eq)

-- DT - Statements val is Batch or Single
data Command = StatementCommand Statements | LoadCommand | SaveCommand
    deriving (Show, Eq)

-- Parses commands - user input
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "LOAD" `isPrefixOf` input = Right (LoadCommand, drop 4 input) -- Returns input without first 4 chars
  | "SAVE" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | "BEGIN" `isPrefixOf` input = case parseStatements input of -- BEGIN means that we have batch. Delegates input to parseStatements
      Left err -> Left err
      Right (gotStatements, rest) -> Right (StatementCommand gotStatements, rest)
  | otherwise = case Lib2.parseQuery (trim input) of -- Case for ADD, DELETE, LIST... - single query therefore just uses Lib2.parseQuery
      Left err -> Left $ "Error parsing query: " ++ err
      Right query -> Right (StatementCommand (Single query), "")



-- Parses statements (single or batch but must be between BEGIN & END)
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let inputTrimmed = trim input
  in if "BEGIN" `isPrefixOf` inputTrimmed -- Checks if it starts with BEGIN
       then
         if "END" `isSuffixOf` inputTrimmed -- Checks if it ends with END
           then
             let body = trim $ drop 5 $ take (length inputTrimmed - 3) inputTrimmed -- Removes BEGIN & END and trims
             in if null body
                  then Right (Batch [], "") -- If there's nothing between BEGIN & END
                  else
                    let commands = map (trim . trimExtra) (splitOn ';' body)
                        parsedCommands = map parseCommandOrStatement commands
                        (errors, queries) = partitionEithers parsedCommands
                    in if null errors
                         then Right (Batch queries, "")
                         else Left $ "Error parsing queries: " ++ show errors
           else Left "Expected 'END' for the end of batch processing." -- If it does not end with END
       else Left "Expected 'BEGIN' for the start of batch processing." -- If it does not start with BEGIN

-- Helper to trim additional characters if necessary
trimExtra :: String -> String
trimExtra = trim . dropWhile (`elem` [';', ' '])


parseCommandOrStatement :: String -> Either String Lib2.Query
parseCommandOrStatement input
  | "SAVE" `isPrefixOf` input = Right Lib2.SaveCommand
  | "LOAD" `isPrefixOf` input = Right Lib2.LoadCommand
  | otherwise = Lib2.parseQuery input



-- Removes unnecessary characters
trim :: String -> String
trim = f . f -- trim x = f (f x)
   where f = reverse . dropWhile (`elem` [' ', '\n'])



-- Used fow splitting when delimiter is ';'
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delimiter cs -- Recursively



-- State to statements (for saving it in a file)
marshallState :: Lib2.State -> Statements -- From Lib2: 'data State = State [Animal]'
marshallState (Lib2.State animals) =
    let addQueries = map Lib2.Add animals -- Applies the Lib2.Add to each Animal in the animals list
    in Batch addQueries



-- Reverse - statements to state
unmarshallState :: Statements -> Lib2.State
unmarshallState (Batch queries) = foldl applyQuery Lib2.emptyState queries -- Initialize emptyState
  where
    applyQuery state query = case Lib2.stateTransition state query of -- Has current state, a single query from the batch
      Right (_, newState) -> newState -- Lib2.stateTransition returs a new state that becomes state
      Left _ -> state -- Current state unchanged
unmarshallState (Single query) = applyQuery Lib2.emptyState query
  where
    applyQuery state query = case Lib2.stateTransition state query of
      Right (_, newState) -> newState
      Left _ -> state



-- Statements to strings
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = "BEGIN\n" ++ renderBatch queries ++ "END\n"

renderBatch :: [Lib2.Query] -> String
renderBatch [] = ""
renderBatch (q:qs) = renderQuery q ++ ";\n" ++ renderBatch qs

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.Add (Lib2.Animal species name age)) = "ADD " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery (Lib2.Delete (Lib2.Animal species name age)) = "DELETE " ++ species ++ " " ++ name ++ " " ++ show age
renderQuery Lib2.ListAnimals = "LIST"
renderQuery (Lib2.CompoundQuery q1 q2) = renderQuery q1 ++ "; " ++ renderQuery q2



-- One big function to update state based on a command
-- TVar Lib2.State - application's current state in TVar 
-- Command - some commmand in process - LoadCommand/SaveCommand/StatementCommand
-- Chan StorageOp - to notify or retrieve results for save/load operationsSAVE
-- IO - performs IO operations (saving/loading)
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, Lib2.State))
stateTransition stateVar command ioChan = case command of

    LoadCommand -> do
        responseChan <- newChan -- New chan for receiving the result of Load performed in storageOpLoop
        writeChan ioChan (Load responseChan) -- Writes to ioChan
        result <- readChan responseChan -- result gets data from responseChan
        case parseStatements result of
            Left err -> return $ Left ("Error loading state: " ++ err)
            Right (loadedStatements, _) -> do
                let loadedState = unmarshallState loadedStatements -- Statements to State
                atomically $ writeTVar stateVar loadedState -- loadedState is written to current state stateVar
                return $ Right (Just "State loaded from file.", loadedState)

    SaveCommand -> do
        (serializedState, currentState) <- atomically $ do
            currentState <- readTVar stateVar
            let statements = marshallState currentState
            let serializedState = renderStatements statements
            return (serializedState, currentState)
        responseChan <- newChan -- ) to receive the result of save from storageOpLoop
        writeChan ioChan (Save serializedState responseChan)
        _ <- readChan responseChan -- Waits for a response from responseChan
        return $ Right (Just "State saved to file.", currentState)


    StatementCommand statements -> do -- Handles the main execution of queries
        atomically $ processStatements stateVar statements
  where
    processStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String, Lib2.State))
    processStatements tvar (Single query) = executeQuerySTM tvar query -- TVar - current state
    processStatements tvar (Batch queries) = do
        initialState <- readTVar tvar
        processBatch initialState queries
      where
        processBatch :: Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String, Lib2.State))
        processBatch _ [] = return $ Right (Just "Batch processed.", Lib2.emptyState)
        processBatch _ (q:qs) = do
            result <- executeQuerySTM tvar q
            case result of
                Left err -> return $ Left err
                Right (msg, newState) -> do
                    rest <- processBatch newState qs
                    case rest of
                        Left err -> return $ Left err
                        Right (restMsg, finalState) ->
                            let combinedMsg = unwords $ filter (not . null) [maybe "" id msg, maybe "" id restMsg]
                            in return $ Right (Just combinedMsg, finalState)

    executeQuerySTM :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String, Lib2.State))
    executeQuerySTM stateVar query = do
        currentState <- readTVar stateVar -- Reads the current state atomically from the shared TVar
        case Lib2.stateTransition currentState query of -- Attempts to apply the query to the current state
            Left err -> return $ Left err
            Right (msg, newState) -> do
                writeTVar stateVar newState -- Updates the TVar atomically
                return $ Right (msg, newState)


-- Parses a plain-text command, processes it and returns a response
processCommand :: TVar Lib2.State -> Chan StorageOp -> String -> IO String
processCommand stateVar ioChan input = case parseCommand input of
    Left err -> return $ "Error: " ++ err
    Right (command, _) -> do
        result <- stateTransition stateVar command ioChan
        return $ case result of
            Left err -> "Error: " ++ err
            Right (msg, _) -> maybe "" id msg

