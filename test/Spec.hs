{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClientDSL
import qualified Lib2
import Control.Monad.Free (Free(..))
import Control.Monad.State (evalState, runState)
import InMemoryInterpreter (interpretInMemory)

-- Test function to run a program in memory
runTest :: Program a -> Lib2.State -> (a, Lib2.State)
runTest program initialState = runState (interpretInMemory program) initialState



main :: IO ()
main = do
    putStrLn "Running InMemoryInterpreter tests:"

    let program1 = do
            addAnimal "Lion" "Leonas" 5
            addAnimal "Tiger" "Tina" 3
            listAnimals
    let (result1, finalState1) = runTest program1 Lib2.emptyState
    putStrLn "Test 1 - Add animals and list:"
    print result1
    print finalState1

    let program2 = do
            addAnimal "Lion" "Leonas" 5
            deleteAnimal "Lion" "Leonas" 5
            listAnimals
    let (result2, finalState2) = runTest program2 Lib2.emptyState
    putStrLn "\nTest 2 - Delete animal:"
    print result2
    print finalState2

    let program3 = do
            addAnimal "Cat" "Mickus" 2
            saveState
            loadState
    let (result3, finalState3) = runTest program3 Lib2.emptyState
    putStrLn "\nTest 3 - Save and load state:"
    print result3 -- "Mock Load Result"
    print finalState3

    let program4 = listAnimals
    let (result4, finalState4) = runTest program4 Lib2.emptyState
    putStrLn "\nTest 4 - List Animals on an empty state:"
    print result4
    print finalState4














-- -- Other tests
-- module Main where

-- import Test.Tasty
-- import qualified Lib2
-- import Test.Tasty.HUnit
-- import Control.Monad.State (runState)
-- import InMemoryInterpreter (interpretInMemory)
-- import ClientDSL (addAnimal, deleteAnimal, listAnimals)

-- main :: IO ()
-- main = defaultMain tests

-- tests :: TestTree
-- tests = testGroup "InMemoryInterpreter Tests"
--     [ testCase "Add and List Animals" $ do
--         let program = do
--                 addAnimal "Lion" "Leo" 5
--                 addAnimal "Tiger" "Tina" 3
--                 listAnimals
--             initialState = Lib2.emptyState
--             (result, finalState) = runState (interpretInMemory program) initialState

--         assertEqual "Final State has 2 animals" 
--             (Lib2.State [Lib2.Animal "Tiger" "Tina" 3, Lib2.Animal "Lion" "Leo" 5]) 
--             finalState

--         assertEqual "ListAnimals result is correct" 
--             ["Tiger Tina 3", "Lion Leo 5"] 
--             result

--     , testCase "Delete an Animal" $ do
--         let program = do
--                 addAnimal "Lion" "Leo" 5
--                 addAnimal "Tiger" "Tina" 3
--                 deleteAnimal "Lion" "Leo" 5
--                 listAnimals
--             initialState = Lib2.emptyState
--             (result, finalState) = runState (interpretInMemory program) initialState

--         assertEqual "Final State has 1 animal" 
--             (Lib2.State [Lib2.Animal "Tiger" "Tina" 3]) 
--             finalState

--         assertEqual "ListAnimals result is correct" 
--             ["Tiger Tina 3"] 
--             result
--     ]
