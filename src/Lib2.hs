module Lib2
    ( Animal(..), 
      Query(..),
      parseAnimal,
      parseString,
      parseNumber,
      parseQuery, 
      parseCompoundQuery,
      parseAdd,
      parseDelete,
      State(..),
      emptyState,
      stateTransition,
      runREPL,
    ) where


import qualified Data.Char as C
import Data.List (isPrefixOf)

-- 1) An entity which represents user input.
data Animal = Animal { species :: String, name :: String, age :: Int }
    deriving (Show, Eq)

data Query
    = Add Animal
    | Delete Animal
    | ListAnimals
    | CompoundQuery Query Query  -- Recursive constructor
    deriving (Show, Eq)

-- <animal> ::= <species> <name> <age>
parseAnimal :: String -> Either String Animal
parseAnimal s = 
    parseString s >>= \(speciesV, rest1) ->
    parseString (dropWhile (== ' ') rest1) >>= \(nameV, rest2) ->
    parseNumber (dropWhile (== ' ') rest2) >>= \(ageV, _) ->
    Right (Animal speciesV nameV ageV)

-- <species> ::= <string>
-- <name> ::= <string>
parseString :: String -> Either String (String, String)
parseString [] = Left "Unexpected end of input"
parseString s = case span C.isAlpha s of
    ("", _) -> Left "Expected string, but got none"
    (str, rest) -> Right (str, rest)

-- <age> ::= <integer>
parseNumber :: String -> Either String (Int, String)
parseNumber s = case span C.isDigit s of
    ("", _) -> Left "Expected number, but got none"
    (numStr, rest) -> Right (read numStr, rest)

-- <command> ::= <add_animal> | <delete_animal> | 'list_animals' | <compound_query>
parseQuery :: String -> Either String Query
parseQuery s 
    | null s = Left "Expected some command but did not get anything"
    | "list_animals" `isPrefixOf` s = Right ListAnimals
    | otherwise = parseCompoundQuery s `orElse` parseAdd s `orElse` parseDelete s

-- <compound_query> ::= <command> ';' <command>
parseCompoundQuery :: String -> Either String Query
parseCompoundQuery s = 
    case break (== ';') s of
        (firstCmd, ';':restCmd) -> do
            query1 <- parseQuery firstCmd
            query2 <- parseQuery (dropWhile (== ' ') restCmd)
            Right (CompoundQuery query1 query2)
        _ -> Left "Expected a compound query, but got a single query."

-- <add_animal> ::= 'ADD' <animal>
parseAdd :: String -> Either String Query
parseAdd s = do
    rest1 <- parseLiteral "ADD " s
    animal <- parseAnimal (dropWhile (== ' ') rest1)
    return (Add animal)

-- <delete_animal> ::= 'DELETE' <species> <name> <age>
parseDelete :: String -> Either String Query
parseDelete s = do
    rest1 <- parseLiteral "DELETE " s
    animal <- parseAnimal (dropWhile (== ' ') rest1)
    return (Delete animal)

-- Checks if the input starts with the given literal (needed for parseAdd and parseDelete)
parseLiteral :: String -> String -> Either String String
parseLiteral literal s 
    | literal `isPrefixOf` s = Right (drop (length literal) s)
    | otherwise = Left ("Did not get a valid command")

-- 4) An entity which represents your program's state.
data State = State [Animal]
    deriving (Show)

-- 5) Creates an initial program's state.
emptyState :: State
emptyState = State []

-- 6) Updates a state according to a query.
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
    then Right (Just "No animals found.", State animals)
    else Right (Just ("Current animals: " ++ show animals), State animals)

-- Handles compound queries
stateTransition state (CompoundQuery q1 q2) = do
    (msg1, newState) <- stateTransition state q1
    (msg2, finalState) <- stateTransition newState q2
    let combinedMsg = unwords $ filter (not . null) [maybe "" id msg1, maybe "" id msg2]
    Right (Just combinedMsg, finalState)

-- Helper to try one parser or another
orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) y = y
orElse x _ = x

-- for testing via terminal
runREPL :: State -> IO ()
runREPL state = do
    putStr ">>> "
    command <- getLine
    case command of
        "exit" -> putStrLn "Exiting the program."
        _ -> do
            case parseQuery command of
                Left err -> putStrLn ("Error: " ++ err)
                Right query -> do
                    -- state transition based on the parsed query
                    case stateTransition state query of
                        Left err -> putStrLn ("Error: " ++ err)
                        Right (msg, newState) -> do
                            putStrLn (maybe "" id msg)
                            runREPL newState  -- continue the loop with the updated state
