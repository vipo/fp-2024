{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )

import Data.Either (isRight)

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
    testCase "Parsing basic hotel variant" $
      isRight (Lib2.parseQuery "ADD. HOTEL: Grand. FLOOR: 1. ROOM: 101. ") @?= True,
    testCase "Parsing incorrect variant" $
      Lib2.parseQuery "ADD. " @?= (Left "Expected a dot (end of query operation), but got end of input."),
    testCase "Parsing add hotel variant with chains and amenities" $
      isRight (Lib2.parseQuery "ADD. HOTEL: Grand. FLOOR: 1. ROOM: 101. AMENITIES: TV, AC, WiFi. ") @?= True,
    testCase "Testing adding and removing hotel variant" $ do
      
      let initialState = Lib2.emptyState

      case Lib2.parseQuery "ADD. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery -> 
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD' query: " ++ err
            Right (_, stateAfterAdd) -> 
              case Lib2.parseQuery "REMOVE. 1. " of
                Left err -> assertFailure $ "Failed to parse 'REMOVE' query: " ++ err
                Right removeQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd removeQuery

                  result @?= Right (Just "Hotel removed successfully!", stateAfterAdd { Lib2.availableHotelEntities = [] }),
    testCase "Testing adding and making a reservation on a hotel room" $ do
      let initialState = Lib2.emptyState
      
      case Lib2.parseQuery "ADD. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery ->
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD query: " ++ err
            Right (_, stateAfterAdd) ->
              case Lib2.parseQuery ("MAKE RESERVATION. GUEST: Elvinukas Testukas. HOTEL: Grand. FLOOR: 1. ROOM: 101. CHECK IN: 2020-01-01 10:00. " ++ 
              "CHECK OUT: 2020-02-02 12:00. PRICE: 210. ") of
                Left err -> assertFailure $ "Failed to parse 'MAKE RESERVATION' query: " ++ err
                Right makeReservationQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd makeReservationQuery
                  case result of
                    Left err -> assertFailure $ "Failed to execute 'MAKE RESERVATION' query: " ++ err
                    Right (_, newState) -> 
                      result @?= Right (Just "Reservation made successfully!", newState),

    testCase "Testing making a reservation and then adding an additional guest" $ do
      let initialState = Lib2.emptyState
      
      case Lib2.parseQuery "ADD. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery ->
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD query: " ++ err
            Right (_, stateAfterAdd) ->
              case Lib2.parseQuery ("MAKE RESERVATION. GUEST: Elvinukas Testukas. HOTEL: Grand. FLOOR: 1. ROOM: 101. CHECK IN: 2020-01-01 10:00. " ++ 
              "CHECK OUT: 2020-02-02 12:00. PRICE: 210. ") of
                Left err -> assertFailure $ "Failed to parse 'MAKE RESERVATION' query: " ++ err
                Right makeReservationQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd makeReservationQuery
                  case result of
                    Left err -> assertFailure $ "Failed to execute 'MAKE RESERVATION' query: " ++ err
                    Right (_, newState) -> do
                      result @?= Right (Just "Reservation made successfully!", newState)
                      (case Lib2.parseQuery "ADD ADDITIONAL GUEST. GUEST: antras testukas. 1. " of
                        Left err -> assertFailure $ "Failed to parse 'ADD ADDITIONAL GUEST' query: " ++ err
                        Right addGuestQuery -> do
                          let result = Lib2.stateTransition newState addGuestQuery
                          case result of
                            Left err -> assertFailure $ "Failed to execute 'ADD ADDITIONAL GUEST' query: " ++ err
                            Right (_, finalState) ->
                              result @?= Right (Just "Guest added successfully!", finalState))

    ]