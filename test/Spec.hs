{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    -- for parseAnimal
    testCase "parseAnimal valid input" $
      Lib2.parseAnimal "monkey Ben 6" @?=
        Right (Lib2.Animal {Lib2.species = "monkey", Lib2.name = "Ben", Lib2.age = 6}),

    testCase "parseAnimal valid input with extra spaces" $
      Lib2.parseAnimal "dog  Max   5  " @?=
        Right (Lib2.Animal {Lib2.species = "dog", Lib2.name = "Max", Lib2.age = 5}),

    testCase "parseAnimal invalid input" $
      Lib2.parseAnimal "12345 Ben 6" @?=
        Left "Expected string, but got none",


    -- for parseString
    testCase "parseString valid input" $
      Lib2.parseString "Animal shelter" @?=
        Right ("Animal", " shelter"),

    testCase "parseString invalid input" $
      Lib2.parseString "123abc" @?=
        Left "Expected string, but got none",


    -- for parseNumber
    testCase "parseNumber valid input" $
      Lib2.parseNumber "123" @?=
        Right (123, ""),

    testCase "parseNumber invalid input" $
      Lib2.parseNumber "abc123" @?=
        Left "Expected number, but got none",


    -- -- for parseQuery     
    testCase "parseQuery valid input for list_animals" $
      Lib2.parseQuery "list_animals" @?=
        Right Lib2.ListAnimals,

    testCase "parseQuery valid input for ADD command" $
      Lib2.parseQuery "ADD cat Tom 3" @?=
        Right (Lib2.Add (Lib2.Animal "cat" "Tom" 3)),

    testCase "parseQuery valid input for DELETE command" $
      Lib2.parseQuery "DELETE dog Max 5" @?=
        Right (Lib2.Delete (Lib2.Animal "dog" "Max" 5)),

    testCase "parseQuery invalid command" $
      Lib2.parseQuery "bad_command" @?=
        Left "Did not get a valid command",

    -- using CompoundQuery
    testCase "parseQuery valid input for compound query" $
      Lib2.parseQuery "ADD cat Tom 3; DELETE dog Max 5" @?=
        Right (Lib2.CompoundQuery 
                (Lib2.Add (Lib2.Animal "cat" "Tom" 3)) 
                (Lib2.Delete (Lib2.Animal "dog" "Max" 5))),


    -- for parseCompoundQuery
    -- testCase "parseCompoundQuery valid input" $
    --   Lib2.parseCompoundQuery "ADD cat Tom 3; DELETE dog Max 5" @?=
    --     Right (Lib2.CompoundQuery 
    --         (Lib2.Add (Lib2.Animal "cat" "Tom" 3)) 
    --         (Lib2.Delete (Lib2.Animal "dog" "Max" 5))),




    -- for parseDelete
    testCase "DELETE valid" $
      Lib2.parseDelete "DELETE dog Max 5" @?= 
        Right (Lib2.Delete (Lib2.Animal "dog" "Max" 5)),

    testCase "DELETE invalid" $
      Lib2.parseDelete "DELETE dog 5" @?= 
        Left "Expected string, but got none"
  ]
