{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, writeTVar, readTVarIO, readTVar, atomically)
import qualified Lib2
import Data.List ( (\\), intercalate, isPrefixOf, partition )

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save queries wchan -> do
      writeFile "data.txt" queries
      writeChan wchan ()
      storageOpLoop chan
    Load rchan -> do
      queries <- readFile "data.txt"
      writeChan rchan queries
      storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

parseCommand :: String -> Either String (Command, String)
parseCommand input = 
  case input of 
  'S':'a':'v':'e':_ -> Right(SaveCommand, "")
  'L':'o':'a':'d':_ -> Right(LoadCommand, "")
  'L':'i':'s':'t':_ -> Right(StatementCommand(Single Lib2.List), "")
  _ -> case parseStatements input of
    Left err -> Left err
    Right(statements, rest) ->Right (StatementCommand statements, rest)

parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
  if isPrefixOf "BEGIN" input  
  then case parseBatch input of
    Right queries -> Right(Batch queries, "")
    Left err -> Left err
  else case Lib2.parseQuery input of 
    Right query -> Right (Single query, "")
    Left err -> Left err

parseBatch :: String -> Either String [Lib2.Query]
parseBatch input = do
  let rows = lines input
  let queries = filter (\line -> not (null line) && line /= "BEGIN" && line /= "END") rows
  mapM Lib2.parseQuery queries

marshallState :: Lib2.State -> Statements
marshallState finalState =
  let
    emptyState = Lib2.State []
    getPlans (Lib2.State plans) = plans
    findAddedPlans initial final = final \\ initial
    generateAddQueries = map Lib2.Add
    initialPlans = getPlans emptyState
    finalPlans = getPlans finalState
    addedPlans = findAddedPlans initialPlans finalPlans
    addQueries = generateAddQueries addedPlans
  in
    if null addQueries
    then Batch []
    else Batch addQueries

renderStatements :: Statements -> String
renderStatements (Batch queries) =
  "BEGIN\n" ++ unlines (map (\(query, i) -> renderQuery query i) (zip queries [1..]))  ++ "END"

renderStatements (Single query) =
  "BEGIN\n" ++ renderQuery query 1 ++ "END"


renderQuery :: Lib2.Query -> Int -> String
renderQuery(Lib2.Add plan) i= 
  renderPlan plan i
renderQuery(Lib2.Delete idx) _ =
  "Delete " ++ show idx
renderQuery(Lib2.Merge idx1 idx2) _ = 
  "Merge " ++ show idx1 ++ " " ++  show idx2
renderQuery Lib2.List _ =
  "List"

renderPlan :: Lib2.Plan -> Int -> String
renderPlan (Lib2.WeekDay days) i =
    unlines $ map renderTuple (zip [1..] days)
  where
    renderTuple (idx, (day, routine)) =
        if idx == 1
            then "Add " ++ day ++ " " ++ renderRoutine routine 
        else
            "Add " ++ day ++ " " ++ renderRoutine routine ++ "\n"
            ++ "Merge " ++ show i ++ " " ++ show (i+1) 

renderRoutine :: Lib2.Routine -> String
renderRoutine (Lib2.Routine exercises) =
  intercalate ", " (map renderExercise exercises)

renderExercise :: Lib2.Exercise -> String
renderExercise (Lib2.Exercise sor name) =
  renderSOR sor ++ " " ++ name 

renderExercise (Lib2.SuperSet routine) =
  "Superset[" ++ renderRoutine routine ++ "]"  

renderSOR :: Lib2.SOR -> String
renderSOR (Lib2.SOR sets reps _) =
  sets ++ "x" ++ reps


stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, String))
stateTransition stateVar command ioChan = case command of
  LoadCommand -> do
    loadChan <- newChan
    writeChan ioChan (Load loadChan)
    info <- readChan loadChan
    case parseStatements info of
      Right (statements, _) -> atomically $ do
        initialState <- readTVar stateVar
        case statements of
          Batch queries -> do
            let applyQueries = foldl (\stateRes query -> case stateRes of
                  Right state -> case Lib2.stateTransition state query of
                    Right (_, newState) -> Right newState
                    Left err -> Left err
                  Left err -> Left err) (Right initialState) queries
            case applyQueries of
              Right newState -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show newState)
              Left err -> return $ Left ("Error applying queries: " ++ err)
          Single query -> do
            case Lib2.stateTransition initialState query of
              Right (_, newState) -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show query)
              Left err -> return $ Left ("Error applying query: " ++ err)
      Left err -> return $ Left ("Error parsing statements: " ++ err)

  SaveCommand -> do
    currentState <- readTVarIO stateVar
    let state = renderStatements (marshallState currentState)
    saveChan <- newChan
    writeChan ioChan (Save state saveChan)
    _ <- readChan saveChan
    return $ Right (Just "State saving was successful.", show currentState)

  StatementCommand statements -> atomically $ do
    currentState <- readTVar stateVar
    case statements of
      Batch queries -> do
        let (addQueries, rest) = partition isAddQuery queries
            (removeQueries, rest') = partition isRemoveQuery rest
            (mergeQueries, rest'') = partition isMergeQuery rest'
            orderedQueries = addQueries ++ mergeQueries ++ removeQueries

        let applyQueries = foldl (\stateRes query -> case stateRes of
              Right state -> case Lib2.stateTransition state query of
                Right (_, newState) -> Right newState
                Left err -> Left err
              Left err -> Left err) (Right currentState) orderedQueries

        case applyQueries of
          Right newState -> do
            writeTVar stateVar newState
            return $ Right (Just "Statements executed successfully.", "Statements executed successfully.")
          Left err -> return $ Left ("Error executing statements: " ++ err)
      Single query -> do
        case query of
          Lib2.List -> do
            currentStateStr <- show <$> readTVar stateVar
            return $ Right (Just "Listed all plans.", currentStateStr)

          _ -> do
            case Lib2.stateTransition currentState query of
              Right (_, newState) -> do
                writeTVar stateVar newState
                return $ Right (Just "Statement executed successfully.", "Statement executed successfully.")
              Left err -> return $ Left ("Error executing statement: " ++ err)

  where
    isAddQuery (Lib2.Add _) = True
    isAddQuery _ = False

    isRemoveQuery (Lib2.Delete _) = True
    isRemoveQuery _ = False

    isMergeQuery (Lib2.Merge _ _) = True
    isMergeQuery _ = False
