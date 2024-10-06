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

    testCase "Correct add make" $
      Lib2.parseQuery "add make honda" @?= Right (Lib2.Query "1honda"),

    testCase "Correct add model" $
      Lib2.parseQuery "add model cbf125" @?= Right (Lib2.Query "2cbf125"),

    testCase "Correct add motorcycle" $
      Lib2.parseQuery "add motorcycle honda cbf125" @?= Right (Lib2.Query "3honda cbf125"),

    testCase "Correct list" $
      Lib2.parseQuery "list" @?= Right (Lib2.Query "4"),

    testCase "add misspelling" $
      Lib2.parseQuery "adds make " @?= Left "Bad input",

    testCase "make misspelling" $
      Lib2.parseQuery "add makes " @?= Left "Bad input",

    testCase "model misspelling" $
      Lib2.parseQuery "add models " @?= Left "Bad input",

    testCase "motorcycle misspelling" $
      Lib2.parseQuery "add motorycles " @?= Left "Bad input"
  ]
