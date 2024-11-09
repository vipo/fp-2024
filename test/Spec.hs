{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    testCase "Parse 'view' query" $
      Lib2.parseQuery "view" @?= Right Lib2.ViewDeck,

    testCase "Parse 'add Ace of Spades' query" $
      Lib2.parseQuery "add Ace of Spades" @?= Right (Lib2.AddDeck (Lib2.SingleCard (Lib2.Card Lib2.Ace Lib2.Spades))),

    testCase "Parse 'delete' query" $
      Lib2.parseQuery "delete" @?= Right Lib2.DeleteDeck,

    testCase "Parse 'Ace of Spades' card" $
      Lib2.parseCard "Ace of Spades" @?= Right (Lib2.Card Lib2.Ace Lib2.Spades, ""),

    testCase "Parse 'Joker' card" $
      Lib2.parseCard "Joker" @?= Right (Lib2.Joker, ""),

    testCase "Parse 'Two of Hearts' card" $
      Lib2.parseCard "Two of Hearts" @?= Right (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts, ""),

    testCase "Parse deck of cards 'Ace of Spades, Two of Hearts'" $
      Lib2.parseDeck "Ace of Spades, Two of Hearts" @?= Right (Lib2.Deck (Lib2.Card Lib2.Ace Lib2.Spades) (Lib2.SingleCard (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts)), "")

  ]

unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Some error message"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]
