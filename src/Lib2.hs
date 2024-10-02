-- Lib2.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib2
    ( Query(..), -- here are specified functions, types, and constructors from this module 
    parseQuery,  -- that are accessible to other modules when they import Lib2
    State(..),   -- (...) - holds export list
    emptyState,
    stateTransition,
    runREPL,      -- Expose the REPL function for testing via terminal
    ) where

import qualified Data.Char as C
import Data.List (isPrefixOf)

type Parser a = String -> Either String (a, String)

-- 1) | An entity which represents user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

-- new data type - Animal
-- "Animal { species :: String, name :: String, age :: Int }" - constructor
data Animal = Animal { species :: String, name :: String, age :: Int }
    deriving (Show, Eq)

-- Query ADT representing commands
data Query
    = Add Animal         -- Command to add an animal
    | Delete Animal      -- Command to delete an animal
    | ListAnimals        -- Command to list all animals
    deriving (Show, Eq)

-- 2) | The instances are needed basically for tests

-- 3) | Parses user's input. The function must have tests.
parseQuery :: String -> Either String Query
parseQuery s 
    | null s = Left "Expected some command but did not get anything"
    | "LIST" `isPrefixOf` s = Right ListAnimals  -- Parse the LIST command
    | otherwise = parseAdd s `orElse` parseDelete s -- tries to use parseAdd, if fails, uses parseDelete

parseAdd :: String -> Either String Query -- if success, right Query; if fail, left String
parseAdd s = do
    rest1 <- parseLiteral "ADD " s -- if s starts w 'ADD', it removes 'ADD' and what is left stores in rest1
    animal <- parseAnimal (dropWhile (== ' ') rest1) -- removes white spaces
    return (Add animal) -- from parseAnimal returns (Animal species name age)

parseDelete :: String -> Either String Query
parseDelete s = do
    rest1 <- parseLiteral "DELETE " s
    animal <- parseAnimal (dropWhile (== ' ') rest1) 
    return (Delete animal)

-- Checks if the input starts with the given literal (command)
parseLiteral :: String -> String -> Either String String
parseLiteral literal s -- (drop (length literal) s) - gives everything after the literal
    | literal `isPrefixOf` s = Right (drop (length literal) s) -- if literal is at the start of s, gives true
    | otherwise = Left ("Expected some command but got '" ++ take (length literal) s ++ "'")

-- Parse an animal (species, name, and age)
parseAnimal :: String -> Either String Animal
parseAnimal s = do
    (species, rest1) <- parseString s
    (name, rest2) <- parseString (dropWhile (== ' ') rest1)
    (age, rest3) <- parseNumber (dropWhile (== ' ') rest2)
    return (Animal species name age)

-- 4) | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- State holds the list of animals
data State = State [Animal]
    deriving (Show)

-- 5) | Creates an initial program's state.
-- It is called once when the program starts.
-- Initial empty state
emptyState :: State
emptyState = State []

-- 6) | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State animals) (Add animal) =
    if animal `elem` animals
    then Left ("Animal " ++ show animal ++ " already exists.")
    else Right (Just ("Added animal: " ++ show animal), State (animal : animals))

stateTransition (State animals) (Delete animal) =
    if animal `elem` animals
    then Right (Just ("Deleted animal: " ++ show animal), State (filter (/= animal) animals))
    else Left ("Animal " ++ show animal ++ " not found.")

stateTransition (State animals) ListAnimals =
    if null animals
    then Right (Just "No animals found.", State animals) -- Handle the case of no animals
    else Right (Just ("Current animals: " ++ show animals), State animals)

-- Helper to try one parser or another
orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) y = y
orElse x _ = x

-- Parse a single character
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left ("Expected " ++ [c] ++ " but got " ++ [h])

-- Parse a digit
parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit (h:t) = if C.isDigit h then Right (h, t) else Left ("Expected digit but got " ++ [h])

-- Parse a number (sequence of digits)
parseNumber :: String -> Either String (Int, String)
parseNumber s = case span C.isDigit s of
    ("", _) -> Left "Expected number, but got none"
    (numStr, rest) -> Right (read numStr, rest)

-- Parse a string (sequence of letters)
parseString :: String -> Either String (String, String)
parseString [] = Left "Unexpected end of input"
parseString s = case span C.isAlpha s of
    ("", _) -> Left "Expected string, but got none"
    (str, rest) -> Right (str, rest)

-- REPL loop to handle user commands
runREPL :: State -> IO ()
runREPL state = do
    putStr ">>> "
    command <- getLine
    case command of
        "exit" -> putStrLn "Exiting the program."
        _ -> do
            -- Parse the input command (ADD, DELETE, etc.)
            case parseQuery command of
                Left err -> putStrLn ("Error: " ++ err)  -- Print parsing errors
                Right query -> do
                    -- Perform the state transition based on the parsed query (ADD, DELETE)
                    case stateTransition state query of
                        Left err -> putStrLn ("Error: " ++ err)  -- Print state transition errors
                        Right (msg, newState) -> do
                            putStrLn (maybe "" id msg)  -- Print success message (e.g., Added animal, Deleted animal)
                            runREPL newState  -- Continue the loop with the updated state
