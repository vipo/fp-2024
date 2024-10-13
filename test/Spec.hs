{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "AddVehicle command parsing" $
        Lib2.parseQuery "add_vehicle(Car, \"ModelX\", 2020, 15000km)"
          @?= Right [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000],
      testCase "PerformMaintenance command parsing" $
        Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2hours)"
          @?= Right [Lib2.PerformMaintenance Lib2.Truck Lib2.OilChange (Lib2.Hours 2)],
      testCase "SellVehicle command parsing" $
        Lib2.parseQuery "sell_vehicle(SUV, \"ModelY\", 2018, 25000.50)"
          @?= Right [Lib2.SellVehicle Lib2.SUV "ModelY" 2018 25000.50],
      testCase "Inventory command parsing" $
        Lib2.parseQuery "inventory(Motorcycle)"
          @?= Right [Lib2.Inventory Lib2.Motorcycle],
      testCase "View command parsing" $
        Lib2.parseQuery "view()"
          @?= Right [Lib2.View],
      testCase "Invalid command parsing" $
        Lib2.parseQuery "invalid command"
          @?= Left "Failed to parse vehicle garage: Expected \"add_vehicle\", but found \"invalid com\"; Expected \"perform_maintenance\", but found \"invalid command\"; Expected \"sell_vehicle\", but found \"invalid comm\"; Expected \"entory\", but found \"alid c\"; Expected \"view\", but found \"inva\"",
      testCase "State transition with AddVehicle" $
        let initialState = Lib2.emptyState
         in case Lib2.parseQuery "add_vehicle(Car, \"ModelX\", 2020, 15000km)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Added Car ModelX (2020)"
                    Lib2.vehicles newState @?= [(Lib2.Car, "ModelX", 2020, 15000)]
                  Left err -> error err
              Left err -> error err,
      testCase "State transition with View" $
        let initialState = Lib2.emptyState
         in case Lib2.stateTransition initialState [Lib2.View] of
              Right (Just stateView, _) -> stateView @?= "Vehicles:\n\nInventory:\n"
              Left err -> error err,
      testCase "State transition with SellVehicle when vehicle exists" $
        let initialState = Lib2.emptyState {Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)]}
         in case Lib2.parseQuery "sell_vehicle(Car, \"ModelX\", 2020, 25000.50)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Sold Car ModelX (2020) for $25000.5"
                    Lib2.vehicles newState @?= []
                  Left err -> error err
              Left err -> error err,
      testCase "State transition with SellVehicle when vehicle does not exist" $
        let initialState = Lib2.emptyState
         in case Lib2.parseQuery "sell_vehicle(Car, \"ModelX\", 2020, 25000.50)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Left err -> err @?= "Vehicle not found in inventory"
                  Right _ -> error "Expected error for non-existent vehicle"
              Left err -> error err,
      testCase "State transition with PerformMaintenance" $
        let initialState = Lib2.emptyState
         in case Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2hours)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Performed OilChange on Truck for Hours 2"
                    newState @?= initialState
                  Left err -> error err
              Left err -> error err,
      testCase "State transition with Inventory" $
        let initialState = Lib2.emptyState {Lib2.vehicles = [(Lib2.Motorcycle, "ModelZ", 2019, 5000)]}
         in case Lib2.parseQuery "inventory(Motorcycle)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, _) -> msg @?= "Inventory for Motorcycle:\nModelZ (2019)\n"
                  Left err -> error err
              Left err -> error err
    ]