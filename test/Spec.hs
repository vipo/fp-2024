{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing empty command" $
      Lib2.parseQuery "" @?= (Left "Expected some command but got ''"),

    testCase "Parsing invalid command" $
      Lib2.parseQuery "o" @?= (Left "Expected 'DELETE ' but got 'o'")
      -- "Expected DELETE but ..." bc in parseQuery it first checks if command is ADD 
      -- and if not, it give error msg with DELETE
  ]