{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements (..),
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import qualified Lib2
import Lib2 (Parser, parseString, runParser)
import System.Directory (doesFileExist)
import Control.Applicative ((<|>), many) 

data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop storageOpChannel = forever $ do
  storageOp <- readChan storageOpChannel
  case storageOp of
    Save string responseChannel -> writeFile fileName string >> writeChan responseChannel ()
    Load responseChannel -> do
      fileExists <- doesFileExist fileName
      if fileExists
        then Just <$> readFile fileName >>= writeChan responseChannel
        else writeChan responseChannel Nothing


fileName :: String
fileName = "state.txt"

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = renderQuery q
  show (Batch b) = "begin\n" ++ concatMap ((++ ";\n") . renderQuery) b ++ "end\n"


renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddReaderQuery (Lib2.ReaderInfo name readerid)) = "add-reader " ++ name ++ " " ++ show readerid
renderQuery (Lib2.AddBookQuery (Lib2.BookInfo title author genre audience)) = "add-book " ++ title ++ " " ++ author ++ " " ++ show genre ++ " " ++ show audience 

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

parseCommand :: String -> Either String (Command, String)
parseCommand = runParser command


parseStatements :: String -> Either String (Statements, String)
parseStatements = runParser statements


marshallState :: Lib2.State -> Statements
marshallState (Lib2.State books readers) = 
  Batch (map Lib2.AddBookQuery books ++ map Lib2.AddReaderQuery readers)


renderStatements :: Statements -> String
renderStatements = show

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition state SaveCommand ioChan = do
  currentState <- readTVarIO state
  responseChan <- newChan
  writeChan ioChan (Save (renderStatements $ marshallState currentState) responseChan)
  readChan responseChan
  return $ Right (Just "State saved successfully")

stateTransition state LoadCommand ioChan = do
  atomically $ writeTVar state Lib2.emptyState
  responseChan <- newChan
  writeChan ioChan (Load responseChan)
  maybeContent <- readChan responseChan
  case maybeContent of
    Nothing -> return $ Left "No state file found"
    Just content -> case parseStatements content of
      Left err -> return $ Left $ "Failed to load from file:\n" ++ err
      Right (parsedStatements, _) -> stateTransition state (StatementCommand parsedStatements) ioChan

stateTransition state (StatementCommand statementList) _ =
  atomically $ atomicStatements state statementList



transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList state (query:remainingQueries) = 
  case Lib2.stateTransition state query of
    Left err -> Left err
    Right (message, newState) ->
      if null remainingQueries
        then Right (message, newState)
        else case transitionThroughList newState remainingQueries of
               Left err -> Left err
               Right (nextMessage, finalState) ->
                 Right (combineMessages message nextMessage, finalState)

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg1) Nothing = Just msg1
combineMessages Nothing (Just msg2) = Just msg2
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)


atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements state statement = do
  currentState <- readTVar state
  case statement of
    Batch queries -> 
      case transitionThroughList currentState queries of
        Left err -> return $ Left err
        Right (message, newState) -> writeTVar state newState >> return (Right message)
    Single query -> 
      case Lib2.stateTransition currentState query of
        Left err -> return $ Left err
        Right (message, newState) -> writeTVar state newState >> return (Right message)



statements :: Parser Statements
statements =
  ( do
      _ <- parseString "begin\n"
      batch <-
        many
          ( do
              query <- Lib2.query
              _ <- parseString ";\n"
              return query
          )
      _ <- parseString "end\n"
      return $ Batch batch
  )
    <|> (Single <$> Lib2.query)

loadParser :: Parser Command
loadParser = do
  _ <- parseString "load"
  return LoadCommand

saveParser :: Parser Command
saveParser = do
  _ <- parseString "save"
  return SaveCommand

command :: Parser Command
command = StatementCommand <$> statements <|> loadParser <|> saveParser