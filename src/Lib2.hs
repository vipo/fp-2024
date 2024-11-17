{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Redundant case" #-}

module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import Text.Read (readMaybe)

-- <cline> ::= <command> <plan>
data Query = Add Plan | Delete Int | List

-- <plan> ::= <weekDay> "(" <routine> ")" | <weekDay> "(" <routine>  ") " <plan> 
data Plan = WeekDay [(String, Routine)]

-- <routine> ::= <exercise> | <exercise> ", " <routine> 
data Routine = Routine [Exercise]

-- <exercise> ::= <sor> <name> | "Superset of [" <routine> "]"
data Exercise = Exercise SOR String | SuperSet Routine

-- <sor> ::= <digits> "x" <digits> "repetitions of "
data SOR = SOR String String Char

type Parser a = String -> Either String (a, String)

instance Show Query where
    show :: Query -> String
    show List = "List"
    show (Add plan) = "Add " ++ show plan
    show (Delete idx) = "Delete " ++ show idx

instance Show Plan where
    show :: Plan -> String
    show (WeekDay days) = unlines $ map (\(day, routine) -> day ++ ": " ++ show routine) days

instance Show Routine where
    show :: Routine -> String
    show (Routine exercises) = L.intercalate ", " (map show exercises)

instance Show Exercise where
    show :: Exercise -> String
    show (Exercise sor name) =  show sor  ++ name 
    show (SuperSet routine) = "Superset of [" ++ show routine ++ "]"

instance Show SOR where
    show :: SOR -> String
    show (SOR sets reps _) = sets ++ "x" ++ reps ++ " repetitions of "

instance Show State where
    show :: State -> String
    show (State created) =
        "Created Plans:\n" ++ listPlans created
      where
        listPlans :: [Plan] -> String
        listPlans = unlines . zipWith (\i plan -> show i ++ ". " ++ show plan) [1..]

parseQuery :: String -> Either String Query
parseQuery input = fmap fst (orElse parseList parseOrder input)
  where
    parseList :: Parser Query
    parseList input' =
        case removeSpaces input' of
            'L':'i':'s':'t':rest -> Right (List, rest)
            _ -> Left "Expected 'List'"

    parseOrder :: Parser Query
    parseOrder input' =
        case removeSpaces input' of
            'A':'d':'d':' ':rest ->
                fmap (\(plan, remaining) -> (Add plan, remaining)) (parsePlan rest)
            'D':'e':'l':'e':'t':'e':' ':rest ->
                case readMaybe (takeWhile C.isDigit rest) of
                    Just idx -> Right (Delete idx, dropWhile C.isDigit rest)
                    Nothing -> Left "Invalid index for Delete"
            _ -> Left "Expected 'Add <plan>' or 'Delete <number>'"

parsePlan :: Parser Plan
parsePlan input =
    let days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
    in parseWeekDay days input
  where
    parseWeekDay :: [String] -> Parser Plan
    parseWeekDay [] _ = Left "No valid weekday found"
    parseWeekDay (day:rest) input' =
        if day `L.isPrefixOf` input'
        then let remaining = drop (length day) input' 
             in case removeSpaces remaining of 
                  _ -> case parseRoutine (removeSpaces remaining) of 
                         Right (routine, rest') -> Right (WeekDay [(day, routine)], rest')
                         Left err -> Left err
        else parseWeekDay rest input'


parseRoutine :: Parser Routine
parseRoutine input =
    let parseExercises :: Parser [Exercise]
        parseExercises input' =
            case parseExercise input' of
                Right (exercise, rest) -> 
                    case removeSpaces rest of
                        ',':' ':rest' -> 
                            fmap (\(exercises, remaining) -> (exercise : exercises, remaining)) (parseExercises rest')
                        _ -> Right ([exercise], rest) 
                Left err -> Left err
    in fmap (\(exercises, remaining) -> (Routine exercises, remaining)) (parseExercises input)

parseExercise :: Parser Exercise
parseExercise input =
    case parseSOR input of
        Right (sor, rest) -> 
            case span (/= ',') (removeSpaces rest) of
                (name, rest') -> Right (Exercise sor name, rest')
                _ -> Left "Expected exercise name after SOR"
        Left err -> 
            if "Superset[" `L.isPrefixOf` input  
            then let inner = drop (length "Superset[") input  
                     inner' = takeWhile (/= ']') inner      
                     rest' = drop (length inner') inner    
                 in case parseRoutine inner' of          
                      Right (routine, rest) -> 
                          if "]" `L.isPrefixOf` rest'      
                          then Right (SuperSet routine, drop 1 rest')  
                          else Left $ "Expected ']' to close Superset |" ++ inner'  
                      Left err' -> Left err'  
            else Left err  

parseSOR :: Parser SOR
parseSOR input =
    let (sets, rest1) = span C.isDigit input
        rest2 = dropWhile C.isLetter rest1
        (reps, rest3) = span C.isDigit rest2
    in if null sets || null reps
       then Left $"Invalid SOR format 1." ++ sets ++ " 2." ++ reps ++ " 3." ++ rest1 
       else Right (SOR sets reps 'x', removeSpaces rest3)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input =
    case p1 input of
        Right res -> Right res
        Left _ -> p2 input

removeSpaces :: String -> String
removeSpaces = dropWhile C.isSpace

type CreatedPlans = [Plan]

data State = State CreatedPlans

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State created) query =
    case query of
        List -> Right (Just (show (State created)), State created)
        
        Add plan ->
            let newState = State (created ++ [plan])
            in Right (Just $ "Added Plan: " ++ show plan, newState)
        
        Delete idx ->
            if idx < 1 || idx > length created
            then Left $ "Invalid index: " ++ show idx
            else 
                let (before, toDelete : after) = splitAt (idx - 1) created  
                in Right (Just $ "Deleted Plan: " ++ show toDelete, State (before ++ after)) 

