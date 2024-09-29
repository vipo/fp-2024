-- Lib2.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib2
    ( Query(..), -- here are specified  functions, types, and constructors from this module 
    parseQuery,  -- that are accessible to other modules when they import Lib2
    State(..),   -- (...) -  holds export list
    emptyState,
    stateTransition
    ) where

import Data.List (isPrefixOf) -- importing isPrefixOf function

-- 1) | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

-- new data type - Animal
-- "Animal { species :: String, name :: String, age :: Int }" - constructor
data Animal = Animal { species :: String, name :: String, age :: Int }
    deriving (Show, Eq)

-- Query ADT representing commands
data Query
    = Add Animal
    | Delete Animal

-- 2) | The instances are needed basically for tests
instance Eq Query where --  any value of type Query can now be compared using ==
    (Add a1) == (Add a2) = a1 == a2 -- if Add and Add, it checks if a1 == a2
    (Delete a1) == (Delete a2) = a1 == a2
    _ == _ = False -- if Query types are different it is automaticaly false

instance Show Query where
    show (Add animal) = "Add " ++ show animal
    show (Delete animal) = "Delete " ++ show animal

-- 3) | Parses user's input. The function must have tests.
parseQuery :: String -> Either String Query
parseQuery s 
    | null s = Left "Expected some command but got ''"
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
    | otherwise = Left ("Expected '" ++ literal ++ "' but got '" ++ take (length literal) s ++ "'")

-- Parse an animal (species, name, and age)
parseAnimal :: String -> Either String Animal
parseAnimal s = do
    (species, rest1) <- string s
    (name, rest2) <- string (dropWhile (== ' ') rest1)
    (age, rest3) <- number (dropWhile (== ' ') rest2)
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