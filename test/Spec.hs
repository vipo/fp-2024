module Main where

import Test.Tasty
import qualified Lib2
import Test.Tasty.HUnit
import Control.Monad.State (runState)
import InMemoryInterpreter (interpretInMemory)
import ClientDSL (addAnimal, deleteAnimal, listAnimals)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "InMemoryInterpreter Tests"
    [ testCase "Add and List Animals" $ do
        let program = do
                addAnimal "Lion" "Leo" 5
                addAnimal "Tiger" "Tina" 3
                listAnimals
            initialState = Lib2.emptyState
            (result, finalState) = runState (interpretInMemory program) initialState

        assertEqual "Final State has 2 animals" 
            (Lib2.State [Lib2.Animal "Tiger" "Tina" 3, Lib2.Animal "Lion" "Leo" 5]) 
            finalState

        assertEqual "ListAnimals result is correct" 
            ["Tiger Tina 3", "Lion Leo 5"] 
            result

    , testCase "Delete an Animal" $ do
        let program = do
                addAnimal "Lion" "Leo" 5
                addAnimal "Tiger" "Tina" 3
                deleteAnimal "Lion" "Leo" 5
                listAnimals
            initialState = Lib2.emptyState
            (result, finalState) = runState (interpretInMemory program) initialState

        assertEqual "Final State has 1 animal" 
            (Lib2.State [Lib2.Animal "Tiger" "Tina" 3]) 
            finalState

        assertEqual "ListAnimals result is correct" 
            ["Tiger Tina 3"] 
            result
    ]

