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
    testCase "Parse incomplete input" $
      Lib2.parseQuery "o" @?= Left "Invalid command format",
    testCase "Parse valid create command" $
      Lib2.parseQuery "create(ball)" @?= Right (Lib2.CreateCommand (Lib2.SimpleToy "ball")),
    testCase "Parse create command with composite toy" $
      Lib2.parseQuery "create(combine(ball,car))" @?= Right (Lib2.CreateCommand (Lib2.CompositeToy [Lib2.SimpleToy "ball", Lib2.SimpleToy "car"])),
    testCase "Parse create command with decorated toy" $
      Lib2.parseQuery "create(decorate(doll,blue))" @?= Right (Lib2.CreateCommand (Lib2.DecoratedToy (Lib2.SimpleToy "doll") (Lib2.Color "blue")))
  ]