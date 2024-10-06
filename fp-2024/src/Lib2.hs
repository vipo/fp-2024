{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = Query String

-- | The instances are needed basically for tests
instance Eq Query where
  (Query a) == (Query b) = a == b

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input
    | null input || all (== ' ') input = Left "Empty String"
    | otherwise = processCommand input

processCommand :: String -> Either String Query
processCommand input =
    let trimmedInput = dropWhile (== ' ') (drop 3 input) 
    in case () of
        _ | isCommandAdd input 0 ->                                 -- Checks if its <add> _ :: "add" "_____" command
            if isCommandMake trimmedInput 0
                then Right (Query (addMakeOrModel input 1))         -- Checks if its <add> <make> :: "add" "make" "____" command
                else if isCommandModel trimmedInput 0
                    then Right (Query (addMakeOrModel input 2))     -- Checks if its <add> <model> :: "add" "model" "____" command
                    else if isCommandMotorcycle trimmedInput 0
                        then Right (Query (addMakeOrModel input 3)) -- Checks if its <add> <motorcycle> :: "add" "motorcycle" "____" "____" command
                        else Left "Bad input"
        
          | isCommandList input 0 ->                                -- Checks if its <list> <_> :: "list" command
            Right (Query "4")
        
          | otherwise -> 
            Left "Bad input"

-- Parses if its <add> command ignores variations of the word like "adds" and etc, checks for space after itself
isCommandAdd :: String -> Int -> Bool
isCommandAdd [] 3 = True
isCommandAdd [] _ = False
isCommandAdd (x:xs) 0
    | x == 'a' = isCommandAdd xs 1
    | otherwise = False
isCommandAdd (x:xs) 1
    | x == 'd' = isCommandAdd xs 2
    | otherwise = False
isCommandAdd (x:xs) 2
    | x == 'd' = isCommandAdd xs 3
    | otherwise = False
isCommandAdd (x:_) 3
    | x == ' ' = True
    | otherwise = False
isCommandAdd _ _ = False

-- Parses if its <make> command ignores variations of the word like "makes" and etc, checks for space after itself
isCommandMake :: String -> Int -> Bool
isCommandMake [] _ = False
isCommandMake (x:xs) 0
    | x == 'm' = isCommandMake xs 1
    | otherwise = False
isCommandMake (x:xs) 1
    | x == 'a' = isCommandMake xs 2
    | otherwise = False
isCommandMake (x:xs) 2
    | x == 'k' = isCommandMake xs 3
    | otherwise = False
isCommandMake (x:xs) 3
    | x == 'e' = isCommandMake xs 4
    | otherwise = False
isCommandMake (x:_) 4
    | x == ' ' = True
    | otherwise = False
isCommandMake _ _ = False

-- Parses if its <model> command ignores variations of the word like "models" and etc, checks for space after itself
isCommandModel :: String -> Int -> Bool
isCommandModel [] _ = False
isCommandModel (x:xs) 0
    | x == 'm' = isCommandModel xs 1
    | otherwise = False
isCommandModel (x:xs) 1
    | x == 'o' = isCommandModel xs 2
    | otherwise = False
isCommandModel (x:xs) 2
    | x == 'd' = isCommandModel xs 3
    | otherwise = False
isCommandModel (x:xs) 3
    | x == 'e' = isCommandModel xs 4
    | otherwise = False
isCommandModel (x:xs) 4
    | x == 'l' = isCommandModel xs 5
    | otherwise = False
isCommandModel (x:_) 5
    | x == ' ' = True
    | otherwise = False
isCommandModel _ _ = False

-- Parses if its <motorcycle> command ignores variations of the word like "motorcycles" and etc, checks for space after itself
isCommandMotorcycle :: String -> Int -> Bool
isCommandMotorcycle [] _ = False
isCommandMotorcycle (x:xs) 0
    | x == 'm' = isCommandMotorcycle xs 1
    | otherwise = False
isCommandMotorcycle (x:xs) 1
    | x == 'o' = isCommandMotorcycle xs 2
    | otherwise = False
isCommandMotorcycle (x:xs) 2
    | x == 't' = isCommandMotorcycle xs 3
    | otherwise = False
isCommandMotorcycle (x:xs) 3
    | x == 'o' = isCommandMotorcycle xs 4
    | otherwise = False
isCommandMotorcycle (x:xs) 4
    | x == 'r' = isCommandMotorcycle xs 5
    | otherwise = False
isCommandMotorcycle (x:xs) 5
    | x == 'c' = isCommandMotorcycle xs 6
    | otherwise = False
isCommandMotorcycle (x:xs) 6
    | x == 'y' = isCommandMotorcycle xs 7
    | otherwise = False
isCommandMotorcycle (x:xs) 7
    | x == 'c' = isCommandMotorcycle xs 8
    | otherwise = False
isCommandMotorcycle (x:xs) 8
    | x == 'l' = isCommandMotorcycle xs 9
    | otherwise = False
isCommandMotorcycle (x:xs) 9
    | x == 'e' = isCommandMotorcycle xs 10
    | otherwise = False
isCommandMotorcycle (x:_) 10
    | x == ' ' = True
    | otherwise = False
isCommandMotorcycle _ _ = False

-- Parses if its <list> command, does not care if anything comes after it, still prints out the states
isCommandList :: String -> Int -> Bool
isCommandList [] 3 = True
isCommandList [] _ = False
isCommandList (x:xs) 0
    | x == 'l' = isCommandList xs 1
    | otherwise = False
isCommandList (x:xs) 1
    | x == 'i' = isCommandList xs 2
    | otherwise = False
isCommandList (x:xs) 2
    | x == 's' = isCommandList xs 3
    | otherwise = False
isCommandList (x:_) 3
    | x == 't' = True
    | otherwise = False
isCommandList _ _ = False

-- int 1 for make, 2 for model, 3 for make+model. Helps parse the make and/or model to put out to the main function only as
-- the value
addMakeOrModel :: String -> Int -> String
addMakeOrModel input 1 = "1" ++ drop (removeUntil input 'e') input
addMakeOrModel input 2 = "2" ++ drop (removeUntil input 'l') input
addMakeOrModel input 3 = "3" ++ drop (removeUntil input 'e') input
addMakeOrModel _ _ = "unknown command"

-- Helper function for addMakeOrModel, helps to remove every character before the values of the make and model or both
removeUntil :: String -> Char -> Int
removeUntil [] _ = 0
removeUntil (x:xs) y
    | x == y = 2  -- If the current character is 'e', return 0 (stop removing)
    | otherwise = 1 + removeUntil xs y  -- Otherwise, count this character and continue

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State
    {
        make :: [String],
        model :: [String]
    }

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State [] []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (Query input) = 
    case getFirstChar input of
        Nothing -> Left "Empty input"
        Just '1' -> 
                let newMake = drop 1 input
                    updatedState = state { make = newMake : make state }
                in Right (Just $ "Added make: " ++ newMake, updatedState)
        Just '2' -> 
                let newModel = drop 1 input
                    updatedState = state { model = newModel : model state }
                in Right (Just $ "Added model: " ++ newModel, updatedState)
        Just '3' -> 
                let newMake = drop 1 (take (removeUntil input ' ' - 2) input)
                    newModel = drop (removeUntil input ' ' - 1) input
                    updatedState = state { make = newMake : make state, model = newModel : model state }
                in Right (Just ("Added motorcycle:\nmake: " ++ newMake ++ "\nmodel: " ++ newModel), updatedState)
        Just '4' ->
            let (makes, models) = accessState state
            in Right (Just ("Current Motorcycles:\nMakes: " ++ makes ++ "\nModels: " ++ models), state)
        _ -> Right (Just ("Wrong command: " ++ input), state)

-- Helps to recognize if its add make model or motorcycle command to the stateTransition, because the input comes 
-- with a number value as a first char according to the type of command
getFirstChar :: String -> Maybe Char
getFirstChar [] = Nothing
getFirstChar (x:_) = Just x

-- Function to access the make and model lists from the state
accessState :: State -> (String, String)
accessState state = 
    let makes = make state
        models = model state
    in (unwords makes, unwords models)