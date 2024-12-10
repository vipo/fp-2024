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
data Query = Print | Add Int
  deriving (Show, Eq)

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = Sum Int

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = Sum 0

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
