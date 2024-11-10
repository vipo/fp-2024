{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "AddVehicle command parsing" $
        Lib2.parseQuery "add_vehicle(Car, \"ModelX\", 2020, 15000km)"
          @?= Right (Lib2.Sequence [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000]),
      testCase "PerformMaintenance command parsing" $
        Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2hours)"
          @?= Right (Lib2.Sequence [Lib2.PerformMaintenance Lib2.Truck Lib2.OilChange (Lib2.Hours 2)]),
      testCase "SellVehicle command parsing" $
        Lib2.parseQuery "sell_vehicle(SUV, \"ModelY\", 2018, 25000.50)"
          @?= Right (Lib2.Sequence [Lib2.SellVehicle Lib2.SUV "ModelY" 2018 25000.50]),
      testCase "Inventory command parsing" $
        Lib2.parseQuery "inventory(Motorcycle)"
          @?= Right (Lib2.Sequence [Lib2.Inventory Lib2.Motorcycle]),
      testCase "View command parsing" $
        Lib2.parseQuery "view()"
          @?= Right (Lib2.Sequence [Lib2.View]),
      testCase "Invalid command parsing" $
        Lib2.parseQuery "invalid command"
          @?= Left "Unrecognized command",
      testCase "State transition with AddVehicle" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "add_vehicle(Car, \"ModelX\", 2020, 15000km)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "\nAdded Car ModelX (2020)"
                Lib2.vehicles newState @?= [(Lib2.Car, "ModelX", 2020, 15000)]
              Left err -> error err
          Left err -> error err,
      testCase "State transition with View" $ do
        let initialState = Lib2.emptyState
        case Lib2.stateTransition initialState Lib2.View of
          Right (Just stateView, _) -> stateView @?= "Vehicles:\n\nInventory:\n"
          Left err -> error err,
      testCase "State transition with SellVehicle when vehicle exists" $ do
        let initialState = Lib2.emptyState {Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)]}
        case Lib2.parseQuery "sell_vehicle(Car, \"ModelX\", 2020, 25000.50)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "\nSold Car ModelX (2020) for $25000.5"
                Lib2.vehicles newState @?= []
              Left err -> error err
          Left err -> error err,
      testCase "State transition with SellVehicle when vehicle does not exist" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "sell_vehicle(Car, \"ModelX\", 2020, 25000.50)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Left err -> err @?= "Vehicle not found in inventory"
              Right _ -> error "Expected error for non-existent vehicle"
          Left err -> error err,
      testCase "State transition with PerformMaintenance" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2hours)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "\nPerformed OilChange on Truck for Hours 2"
                newState @?= initialState
              Left err -> error err
          Left err -> error err,
      testCase "State transition with Inventory" $ do
        let initialState = Lib2.emptyState {Lib2.vehicles = [(Lib2.Motorcycle, "ModelZ", 2019, 5000)]}
        case Lib2.parseQuery "inventory(Motorcycle)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, _) -> msg @?= "\nInventory for Motorcycle:\nModelZ (2019)\n"
              Left err -> error err
          Left err -> error err,
      testCase "State transition with Sequence" $ do
        let initialState = Lib2.emptyState
            queries = [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000, Lib2.AddVehicle Lib2.Truck "ModelY" 2019 20000]
        case Lib2.stateTransition initialState (Lib2.Sequence queries) of
          Right (Just msg, newState) -> do
            msg @?= "\nAdded Car ModelX (2020)\nAdded Truck ModelY (2019)"
            Lib2.vehicles newState @?= [(Lib2.Truck, "ModelY", 2019, 20000), (Lib2.Car, "ModelX", 2020, 15000)]
          Left err -> error err
    ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]
