{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit (assertBool,testCase, (@?=), assertFailure)
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord


import Test.Tasty
import Control.Monad (when)

import Lib2

import Parsers
import Data.Maybe (fromJust)
import qualified Data.Char as C
import qualified Data.List as L
import Lib3


instance Arbitrary Number where
    arbitrary = elements [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten]

instance Arbitrary Rank where
    arbitrary = oneof [ RankNumber <$> arbitrary
                     , elements [Jack, Queen, King, Ace]
                     ]

instance Arbitrary Suit where
    arbitrary = elements [Hearts, Diamonds, Clubs, Spades]

instance Arbitrary Card where
    arbitrary = oneof [ Card <$> arbitrary <*> arbitrary
                      , pure Joker
                      ]

instance Arbitrary Deck where
    arbitrary = oneof [ SingleCard <$> arbitrary
                     , Deck <$> arbitrary <*> arbitrary
                     ]

instance Arbitrary Query where
    arbitrary = oneof [ pure ViewDeck
                      , AddDeck <$> arbitrary
                      , pure DeleteDeck
                      ]

instance Arbitrary Statements where
    arbitrary = oneof [ Single <$> arbitrary
                     , Batch <$> arbitrary
                     ]
main :: IO ()
main =  defaultMain tests

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
      let (result, _) = parse parseCard "Ace of Spades"
      in case result of
          Right (Lib2.Card Lib2.Ace Lib2.Spades) -> assertBool "Card is correctly parsed" True
          _ -> assertBool "Card is not parsed correctly" False,


    testCase "Parse 'Joker' card" $
      let (result, _) = parse parseCard "Joker"
      in case result of
          Right Lib2.Joker -> assertBool "Joker is correctly parsed" True
          _ -> assertBool "Joker is not parsed correctly" False,

    testCase "Parse 'Two of Hearts' card" $
      let (result, _) = parse parseCard "Two of Hearts"
      in case result of
          Right (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts) -> assertBool "Card is correctly parsed" True
          _ -> assertBool "Card is not parsed correctly" False,

    testCase "Parse deck of cards 'Ace of Spades, Two of Hearts'" $
      let (result, _) = parse parseDeck "Ace of Spades, Two of Hearts"
      in case result of
          Right (Lib2.Deck (Lib2.Card Lib2.Ace Lib2.Spades) (Lib2.SingleCard (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts))) -> assertBool "Deck is correctly parsed" True
          _ -> assertBool "Deck is not parsed correctly" False

  ]


propertyTests :: TestTree
propertyTests = testGroup "Random Property Tests"
  [
    testProperty "renderStatements and parseStatements are inverses" $ 
      \stmt -> 
        let rendered = renderStatements stmt
            (parsed, _) = parse Lib3.parseStatements rendered
        in case parsed of
            Left _ -> False
            Right parsedStmt -> stmt == parsedStmt
  ]

