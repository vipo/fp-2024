{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Redundant case" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib2
    ( 
      Query(..),
      State(..),
      parseQuery,
      Plan(..),
      Routine(..),
      Exercise(..),
      SOR(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import Text.Read (readMaybe)

instance Eq Query where
      (==) :: Query -> Query -> Bool
      (==) q1 q2 = show q1 == show q2

instance Eq Plan where
      (==) :: Plan -> Plan -> Bool
      (==) q1 q2 = show q1 == show q2

-- <cline> ::= <command> <plan>
data Query = Add Plan | Delete Int | Merge Int Int | List 

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
    show (Merge idx1 idx2) = "Merge " ++ show idx1 ++ " " ++ show idx2

instance Show Plan where
    show :: Plan -> String
    show (WeekDay days) = 
        let planStrs = map (\(day, routine) -> day ++ ": " ++ show routine) days
        in L.intercalate ";\n   " planStrs ++ ";"

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
parseQuery input = fmap fst (orElse parseList (orElse parseMerge parseOrder) input)
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
                let (planStr, remaining) = break (== ';') rest
                in case parsePlan planStr of
                    Right (plan, _) -> Right (Add plan, drop 1 remaining)
                    Left err -> Left err
            'D':'e':'l':'e':'t':'e':' ':rest ->
                case readMaybe (takeWhile C.isDigit rest) of
                    Just idx -> Right (Delete idx, dropWhile C.isDigit rest)
                    Nothing -> Left "Invalid index for Delete"
            _ -> Left "Expected 'Add <plan>' or 'Delete <number>'"

    parseMerge :: Parser Query
    parseMerge input' =
        case removeSpaces input' of
            'M':'e':'r':'g':'e':' ':rest ->
                let (idx1Str, rest') = span C.isDigit rest
                    (idx2Str, rest'') = span C.isDigit (removeSpaces rest')
                in case (readMaybe idx1Str, readMaybe idx2Str) of
                    (Just idx1, Just idx2) -> Right (Merge idx1 idx2, rest'')
                    _ -> Left "Invalid indices for Merge"
            _ -> Left "Expected 'Merge <index1> <index2>'"

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
    fmap (\(exercises, rest) -> (Routine exercises, rest)) (parseExercises input)
    where
    parseExercises :: Parser [Exercise]
    parseExercises =
        and3' combine parseExercise parseCommaOrEnd parseOptionalExercises

    combine :: Exercise -> Maybe Char -> Maybe [Exercise] -> [Exercise]
    combine exercise _ Nothing = [exercise]
    combine exercise _ (Just moreExercises) = exercise : moreExercises

    parseCommaOrEnd :: Parser (Maybe Char)
    parseCommaOrEnd input' =
        case removeSpaces input' of
            ',':' ':rest -> Right (Just ',', rest)
            _            -> Right (Nothing, input')

    parseOptionalExercises :: Parser (Maybe [Exercise])
    parseOptionalExercises input' =
        case parseExercises input' of
            Right (exercises, rest) -> Right (Just exercises, rest)
            Left _                  -> Right (Nothing, input')


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
        
and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

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
        
        Merge idx1 idx2 -> 
            if idx1 < 1 || idx1 > length created || idx2 < 1 || idx2 > length created || idx1 == idx2
            then Left $ "Invalid indices: " ++ show idx1 ++ ", " ++ show idx2
            else
                let idx1' = min idx1 idx2 
                    idx2' = max idx1 idx2
                    (before1, plan1 : after1) = splitAt (idx1' - 1) created
                    (before2, plan2 : after2) = splitAt (idx2' - idx1' - 1) after1
                    mergedPlan = mergePlans plan1 plan2
                    newState = State (before1 ++ [mergedPlan] ++ before2 ++ after2)
                in Right (Just $ "Merged Plans: " ++ show idx1 ++ " and " ++ show idx2, newState)
        
        Delete idx ->
            if idx < 1 || idx > length created
            then Left $ "Invalid index: " ++ show idx
            else 
                let (before, toDelete : after) = splitAt (idx - 1) created  
                in Right (Just $ "Deleted Plan: " ++ show toDelete, State (before ++ after)) 

mergePlans :: Plan -> Plan -> Plan
mergePlans (WeekDay days1) (WeekDay days2) = WeekDay (days1 ++ days2)