-- {-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use newtype instead of data" #-}
-- {-# LANGUAGE NondecreasingIndentation #-}

-- module Lib2
--     ( Animal(..),
--       Query(..),
--       parseAnimal,
--       parseChar,
--       parseString,
--       parseNumber,
--       parseQuery,
--       parseCompoundQuery,
--       parseAdd,
--       parseDelete,
--       parseList,
--       State(..),
--       emptyState,
--       stateTransition,
--     ) where

-- import Control.Monad.Trans.Except (ExceptT, throwE)
-- import qualified Control.Monad.Trans.State as S
-- import Control.Monad.Trans.State (get, put, modify)
-- import Control.Monad (when)
-- import qualified Data.Char as C

-- -- Type for a parser
-- type Parser a = ExceptT String (S.State String) a

-- -- 1) An entity which represents user input.
-- data Animal = Animal { species :: String, name :: String, age :: Int }
--     deriving (Show, Eq)

-- instance Ord Animal where
--     compare (Animal s1 n1 a1) (Animal s2 n2 a2) =
--         compare (s1, n1, a1) (s2, n2, a2)

-- data Query
--     = Add Animal
--     | Delete Animal
--     | ListAnimals
--     | SaveCommand
--     | LoadCommand
--     | CompoundQuery Query Query
--     deriving (Show, Eq)

-- -- <animal> ::= <species> <name> <age>
-- parseAnimal :: Parser Animal
-- parseAnimal = do
--     species <- parseString
--     _ <- parseChar ' '
--     name <- parseString
--     _ <- parseChar ' '
--     age <- parseNumber
--     return $ Animal species name age

-- -- Parses a single character
-- parseChar :: Char -> Parser Char
-- parseChar c = do
--     input <- get
--     case input of
--         [] -> throwE "Unexpected end of input"
--         (h:t) -> 
--             if h == c
--                 then put t >> return h
--                 else throwE $ "Expected '" ++ [c] ++ "', but got '" ++ [h] ++ "'"

-- -- Parses a string
-- parseString :: Parser String
-- parseString = do
--     input <- get
--     let (letters, rest) = span C.isAlpha input
--     when (null letters) $ throwE "Expected a string, but none found"
--     put rest
--     return letters


-- parseNumber :: Parser Int
-- parseNumber = do
--     input <- get
--     let (digits, rest) = span C.isDigit input
--     when (null digits) $ throwE "Expected a number, but none found"
--     put rest
--     return (read digits)


-- parseQuery :: Parser Query
-- parseQuery = parseList `orElseP`
--              parseCompoundQuery `orElseP`
--              parseAdd `orElseP`
--              parseDelete `orElseP` do
--                 literal "SAVE"
--                 return SaveCommand `orElseP` do
--                 literal "LOAD"
--                 return LoadCommand


-- parseCompoundQuery :: Parser Query
-- parseCompoundQuery = do
--     query1 <- parseQuery
--     _ <- parseChar ';'
--     query2 <- parseQuery
--     return $ CompoundQuery query1 query2


-- parseAdd :: Parser Query
-- parseAdd = do
--     literal "ADD"
--     _ <- parseChar ' '
--     animal <- parseAnimal
--     return $ Add animal


-- parseDelete :: Parser Query
-- parseDelete = do
--     literal "DELETE"
--     _ <- parseChar ' '
--     animal <- parseAnimal
--     return $ Delete animal


-- parseList :: Parser Query
-- parseList = do
--     literal "LIST"
--     return ListAnimals


-- literal :: String -> Parser ()
-- literal [] = return ()
-- literal (c:cs) = do
--     _ <- parseChar c
--     literal cs


-- orElseP :: Parser a -> Parser a -> Parser a
-- orElseP p1 p2 = p1 `catchE` (const p2)


-- data State = State [Animal]
--     deriving (Show, Eq)

-- emptyState :: State
-- emptyState = State []

-- -- Updats a state based by a query
-- stateTransition :: State -> Query -> Either String (Maybe String, State)
-- stateTransition (State animals) (Add animal) =
--     if animal `elem` animals
--         then Left ("Animal " ++ show animal ++ " already exists.")
--         else Right (Just ("Added animal: " ++ show animal), State (animal : animals))

-- stateTransition (State animals) (Delete animal) =
--     if animal `elem` animals
--         then Right (Just ("Deleted animal: " ++ show animal), State (filter (/= animal) animals))
--         else Left ("Animal " ++ show animal ++ " not found.")

-- stateTransition (State animals) SaveCommand =
--     Right (Just "\n\n-----State saved successfully-----\n", State animals)

-- stateTransition (State animals) LoadCommand =
--     Right (Just "State loaded successfully.", State animals)

-- stateTransition (State animals) ListAnimals =
--     if null animals
--         then Right (Just "No animals found.", State animals)
--         else Right (Just ("\nCurrent animals: " ++ show animals), State animals)

-- stateTransition state (CompoundQuery q1 q2) =
--     case stateTransition state q1 of
--         Left err -> Left err
--         Right (msg1, newState) ->
--             case stateTransition newState q2 of
--                 Left err -> Left err
--                 Right (msg2, finalState) ->
--                     let combinedMsg = unwords $ filter (not . null) [maybe "" id msg1, maybe "" id msg2]
--                     in Right (Just combinedMsg, finalState)


-- main :: IO ()
-- main = do
--     let runParser p input = runState (runExceptT p) input
--     print $ runParser parseAnimal "giraffe Lin 10"   -- Right (Animal "giraffe" "Lin" 10)
--     print $ runParser parseQuery "ADD dog Rex 5"    -- Right (Add (Animal "dog" "Rex" 5))
--     print $ runParser parseQuery "DELETE cat Murr 4" -- Right (Delete (Animal "cat" "Murr" 4))
--     print $ runParser parseQuery "LIST"             -- Right ListAnimals
--     print $ runParser parseQuery "SAVE"             -- Right SaveCommand
--     print $ runParser parseQuery "LOAD"             -- Right LoadCommand
