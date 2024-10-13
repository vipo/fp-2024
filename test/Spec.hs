{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    -- Test query parsing
    testCase "Parse 'view' query" $
      Lib2.parseQuery "view" @?= Right Lib2.ViewDeck,

    testCase "Parse 'view   ' with spaces" $
      Lib2.parseQuery "view   " @?= Right Lib2.ViewDeck,

    testCase "Parse 'add Ace of Spades' query" $
      Lib2.parseQuery "add Ace of Spades" @?= Right (Lib2.AddDeck (Lib2.SingleCard (Lib2.Card Lib2.Ace Lib2.Spades))),

    testCase "Parse 'delete' query" $
      Lib2.parseQuery "delete" @?= Right Lib2.DeleteDeck,

    testCase "Parse invalid query" $
      Lib2.parseQuery "invalid command" @?= Left "Invalid command",

    -- Test state transitions
    testCase "Add a card to the deck" $ do
      let initialState = Lib2.emptyState
      let cardToAdd = Lib2.Card Lib2.Ace Lib2.Spades
      let expectedState = Lib2.State (Just (Lib2.SingleCard cardToAdd))
      let result = Lib2.stateTransition initialState (Lib2.AddDeck (Lib2.SingleCard cardToAdd))
      case result of
        Right (Just message, newState) -> do
          message @?= "Card added."
          newState @?= expectedState
        Left err -> assertFailure $ "Failed to add card: " ++ err,

    testCase "ViewDeck empty deck" $ 
      let result = Lib2.stateTransition Lib2.emptyState Lib2.ViewDeck
      in result @?= Right (Just "The deck is empty.", Lib2.emptyState),

    testCase "Delete the deck" $ 
      let addResult = Lib2.stateTransition Lib2.emptyState (Lib2.AddDeck (Lib2.SingleCard (Lib2.Card Lib2.Ace Lib2.Spades)))
      in case addResult of
           Right (_, stateWithCard) -> 
             let deleteResult = Lib2.stateTransition stateWithCard Lib2.DeleteDeck
             in deleteResult @?= Right (Just "Deck deleted.", Lib2.emptyState)
           Left err -> assertFailure $ "Failed to add card: " ++ err
  ]
