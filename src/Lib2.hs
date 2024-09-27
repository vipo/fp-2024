{-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Unused LANGUAGE pragma" #-}
-- {-# OPTIONS_GHC -Wno-dodgy-exports #-}
-- {-# HLINT ignore "Use newtype instead of data" #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib2

    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import Data.List (isPrefixOf) -- importing isPrefixOf function

-- 1) | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

-- Animal type definition
data Animal = Animal { species :: String, name :: String, age :: Int }
    deriving (Show, Eq)

  -- Query ADT representing commands
data Query
    = Add Animal        -- Add an animal
    | Delete Animal     -- Delete an animal

-- 2) | The instances are needed basically for tests
instance Eq Query where
    (Add a1) == (Add a2) = a1 == a2         -- Two Add queries are equal if their animals are equal
    (Delete a1) == (Delete a2) = a1 == a2   -- Two Delete queries are equal if their animals are equal
    _ == _ = False                          -- Different types of queries are not equal

instance Show Query where
    show (Add animal) = "Add " ++ show animal
    show (Delete animal) = "Delete " ++ show animal

-- 3) | Parses user's input.
-- The function must have tests.
-- Parse a command (either ADD or DELETE)
parseQuery :: String -> Either String Query
parseQuery s 
    | null s = Left "Expected 'ADD ' but got ''"  -- Handle empty input
    | otherwise = parseAdd s `orElse` parseDelete s


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
-- Transition function to update state based on a Query
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State animals) (Add animal) =
    if animal `elem` animals
    then Left ("Animal " ++ show animal ++ " already exists.")
    else Right (Just ("Added animal: " ++ show animal), State (animal : animals))

stateTransition (State animals) (Delete animal) =
    if animal `elem` animals
    then Right (Just ("Deleted animal: " ++ show animal), State (filter (/= animal) animals))
    else Left ("Animal " ++ show animal ++ " not found.")


-- Helper to try one parser or another
orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) y = y
orElse x _ = x



-- Parse a single character
char :: Char -> String -> Either String (Char, String)
char _ [] = Left "Unexpected end of input"
char c (x:xs) = if c == x then Right (c, xs) else Left ("Expected '" ++ [c] ++ "', but got '" ++ [x] ++ "'")

-- Parse a digit
digit :: String -> Either String (Char, String)
digit [] = Left "Unexpected end of input"
digit (x:xs)
    | x `elem` ['0'..'9'] = Right (x, xs)
    | otherwise = Left ("Expected digit, but got '" ++ [x] ++ "'")

-- Parse a number (sequence of digits)
number :: String -> Either String (Int, String)
number s = case span (`elem` ['0'..'9']) s of
    ("", _) -> Left "Expected number, but got none"
    (numStr, rest) -> Right (read numStr, rest)

-- Parse a string (sequence of letters)
string :: String -> Either String (String, String)
string [] = Left "Unexpected end of input"
string s = case span (`elem` ['a'..'z'] ++ ['A'..'Z']) s of
    ("", _) -> Left "Expected string, but got none"
    (str, rest) -> Right (str, rest)

-- Parse an animal (species, name, and age)
parseAnimal :: String -> Either String Animal
parseAnimal s = do
    (species, rest1) <- string s
    (name, rest2) <- string (dropWhile (== ' ') rest1)
    (age, rest3) <- number (dropWhile (== ' ') rest2)
    return (Animal species name age)

-- Parse the 'ADD' command
parseAdd :: String -> Either String Query
parseAdd s = do
    rest1 <- parseLiteral "ADD " s
    animal <- parseAnimal (dropWhile (== ' ') rest1)
    return (Add animal)

-- Parse the 'DELETE' command
parseDelete :: String -> Either String Query
parseDelete s = do
    rest1 <- parseLiteral "DELETE " s
    animal <- parseAnimal (dropWhile (== ' ') rest1)
    return (Delete animal)

-- Parse a literal string (checks if the input starts with the given literal)
parseLiteral :: String -> String -> Either String String
parseLiteral literal s
    | literal `isPrefixOf` s = Right (drop (length literal) s)
    | otherwise = Left ("Expected '" ++ literal ++ "' but got '" ++ take (length literal) s ++ "'")
