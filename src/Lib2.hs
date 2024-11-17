{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib2
    ( Animal(..), 
      Query(..),
      parseAnimal,
      parseChar,
      parseString,
      parseNumber,
      parseQuery, 
      parseCompoundQuery,
      parseAdd,
      parseDelete,
      parseList,
      State(..),
      emptyState,
      stateTransition,
    ) where


import qualified Data.Char as C

-- 1) An entity which represents user input.
data Animal = Animal { species :: String, name :: String, age :: Int }
    deriving (Show, Eq)

data Query
    = Add Animal
    | Delete Animal
    | ListAnimals
    | CompoundQuery Query Query  -- recursive constructor
    deriving (Show, Eq)

-- <animal> ::= <species> <name> <age>
parseAnimal :: String -> Either String Animal
parseAnimal s =
  case parseString s of
    Left err -> Left err -- if error, returns error msg
    Right (speciesV, rest1) -> 
      case parseString (dropWhile (== ' ') rest1) of
        Left err -> Left err
        Right (nameV, rest2) -> 
          case parseNumber (dropWhile (== ' ') rest2) of
            Left err -> Left err
            Right (ageV, _) -> 
              Right (Animal speciesV nameV ageV)



-- needed in other parsers
parseChar :: Char -> String -> Either String String
parseChar _ [] = Left "Unexpected end of input"
parseChar c (h:t)
  | c == h    = Right t
  | otherwise = Left ("Expected " ++ [c] ++ " but got " ++ [h])


-- <species> ::= <string>
-- <name> ::= <string>
parseString :: Parser String -- just like 'String -> Either String (String, String)'
parseString [] = Left "Expected string, but got an empty input"
parseString s@(h:_)
    | C.isAlpha h = Right (letters, rest)
    | otherwise = Left (s ++ " does not start with an alphabetic character")
  where
    (letters, rest) = span C.isAlpha s -- 'letters' is the longest prefix that is true for C.isAlpha

-- >>> parseNumber "avsd2"
-- Left "avsd2 does not start with a digit"
parseNumber :: Parser Int -- just like 'String -> Either String (Int, String)'
parseNumber [] = Left "Expected number, but got an empty input"
parseNumber s@(h:_)
    | C.isDigit h = Right (read digits, rest) -- ?
    | otherwise = Left (s ++ " does not start with a digit")
  where
    (digits, rest) = span C.isDigit s

type Parser a = String -> Either String (a, String)

-- <command> ::= <add_animal> | <delete_animal> | 'LIST' | <compound_query>
parseQuery :: String -> Either String Query
parseQuery s 
    | null s = Left "Expected some command but did not get anything"
    | otherwise = parseList s `orElse` parseCompoundQuery s `orElse` parseAdd s `orElse` parseDelete s


-- <compound_query> ::= <command> ';' <command>
parseCompoundQuery :: String -> Either String Query
parseCompoundQuery s = 
    case break (== ';') s of
        (firstCmd, ';':restCmd) ->
            parseQuery firstCmd `andThen` \query1 ->
            parseQuery (dropWhile (== ' ') restCmd) `andThen` \query2 ->
            Right (CompoundQuery query1 query2)
        _ -> Left "Expected a compound query, but got a single query."

-- <add_animal> ::= 'ADD' <animal>
parseAdd :: String -> Either String Query
parseAdd s =
    parseLiteral "ADD " s `andThen` \rest1 ->
    parseAnimal (dropWhile (== ' ') rest1) `andThen` \animal ->
    Right (Add animal)

-- <delete_animal> ::= 'DELETE' <species> <name> <age>
parseDelete :: String -> Either String Query
parseDelete s =
    parseLiteral "DELETE " s `andThen` \rest1 ->
    parseAnimal (dropWhile (== ' ') rest1) `andThen` \animal ->
    Right (Delete animal)

-- <list_animals> ::= 'LIST'
parseList :: String -> Either String Query
parseList s = 
    parseLiteral "LIST" s `andThen` \_ ->
    Right ListAnimals

-- Checks if the input starts w the given literal (needed for parseAdd and parseDelete)
parseLiteral :: String -> String -> Either String String
parseLiteral [] s = Right s  -- All characters matched, return remaining string
parseLiteral (c:cs) s = 
  case parseChar c s of
    Left err -> Left err  -- If the character doesn't match, return an error
    Right rest -> parseLiteral cs rest  -- If it matches, continue with the rest of the literal


-- 4) An entity which represents your program's state.
data State = State [Animal]
    deriving (Show, Eq)

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
stateTransition state (CompoundQuery q1 q2) =
  case stateTransition state q1 of
    Left err -> Left err
    Right (msg1, newState) ->
      case stateTransition newState q2 of
        Left err -> Left err 
        Right (msg2, finalState) ->
          let combinedMsg = unwords $ filter (not . null) [maybe "" id msg1, maybe "" id msg2]
          in Right (Just combinedMsg, finalState)  -- If succes, combines messages and returns the final state


-- Helpers
andThen :: Either String a -> (a -> Either String b) -> Either String b
andThen (Left err) _ = Left err
andThen (Right val) f = f val

orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) alt = alt
orElse result _ = result


main :: IO ()
main = do
  print (parseAnimal "giraffe Lin 10") -- output: Right (Animal {species = "giraffe", name = "Lin", age = 10})
  print (parseString "HereLives5Animals") -- output: Right ("HereLives","5Animals")
  print (parseNumber "30Dogs") -- output: Right (30,"Dogs")
  print (parseQuery "ADD cat Mino 4") -- output: Right (Add (Animal {species = "cat", name = "Mino", age = 4}))
  print (parseCompoundQuery "ADD hamster Lili 1; DELETE hamster Lili 1") -- output: Right (CompoundQuery (Add (Animal {species = "hamster", name = "Lili", age = 1})) (Delete (Animal {species = "hamster", name = "Lili", age = 1})))
  print (parseAdd "ADD fish Jojo 1") -- output: Right (Add (Animal {species = "fish", name = "Jojo", age = 1}))
  print (parseDelete "DELETE cat Mo 4") -- output: Right (Delete (Animal {species = "cat", name = "Mo", age = 4}))
