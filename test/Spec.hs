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

    testCase "Parsing empty query" $
     Lib2.parseQuery "" @?= Left "No parser matched",


    testCase "Parsing AddReaderQuery" $
    Lib2.parseQuery "add-reader John 123" @?= 
      Right (Lib2.AddReaderQuery 
        (Lib2.ReaderInfo "John" 123)
      ),
        
    testCase "Parsing invalid command" $
      Lib2.parseQuery "invalidCommand" @?= Left "No parser matched"
  ]
