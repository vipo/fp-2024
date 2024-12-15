{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InMemoryInterpreter (interpretInMemory) where

import ClientDSL
import qualified Lib2
import Control.Monad.Free
import Data.List (isPrefixOf)
import Control.Monad.State (State, get, put)


interpretInMemory :: Program a -> State Lib2.State a
interpretInMemory (Free (AddAnimal sp n a next)) = do
    currentState <- get
    let result = Lib2.stateTransition currentState (Lib2.Add (Lib2.Animal sp n a))
    case result of
        Left err -> error err
        Right (_, newState) -> do
            put newState
            interpretInMemory next

interpretInMemory (Free (DeleteAnimal sp n a next)) = do
    currentState <- get
    let result = Lib2.stateTransition currentState (Lib2.Delete (Lib2.Animal sp n a))
    case result of
        Left err -> error err
        Right (_, newState) -> do
            put newState
            interpretInMemory next

interpretInMemory (Free (ListAnimals next)) = do
    currentState <- get
    let result = Lib2.stateTransition currentState Lib2.ListAnimals
    case result of
        Left err -> error err
        Right (Just msg, _) -> do
            let formattedList = extractAnimalList msg
            interpretInMemory (next formattedList)

interpretInMemory (Free (SaveState next)) = do
    interpretInMemory next

interpretInMemory (Free (LoadState next)) = do
    interpretInMemory (next "Mock Load Result")

interpretInMemory (Pure x) = return x

extractAnimalList :: String -> [String]
extractAnimalList msg =
    if "Current animals:" `isPrefixOf` msg
        then let content = drop (length "Current animals: ") msg
             in lines content
        else []
