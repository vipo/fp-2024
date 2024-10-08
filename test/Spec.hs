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
unitTests = testGroup "Lib2 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "State transition with empty query" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Unknown command"
            Right _ -> error "Expected error for empty query"
        Left err -> err @?= "Unknown command",
    testCase "State transition with invalid command" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "o" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Unknown command"
            Right _ -> error "Expected error for invalid command"
        Left err -> err @?= "Unknown command",
    testCase "State transition with add_vehicle and valid input" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "add_vehicle Car ModelX 2020 15000" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.vehicles newState @?= [(Lib2.Car, "ModelX", 2020, 15000)]
            Left err -> error err
        Left err -> error err,
    testCase "State transition with add_vehicle and invalid vehicle type" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "add_vehicle Plane ModelX 2020 15000" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Failed to parse add_vehicle"
            Right _ -> error "Expected error for invalid vehicle type"
        Left err -> err @?= "Failed to parse add_vehicle",
    testCase "State transition with perform_maintenance and valid input" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "perform_maintenance Car OilChange 5 hours" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.vehicles newState @?= []
            Left err -> error err
        Left err -> error err,
    testCase "State transition with perform_maintenance and invalid maintenance type" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "perform_maintenance Car Wash 5 hours" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Failed to parse perform_maintenance"
            Right _ -> error "Expected error for invalid maintenance type"
        Left err -> err @?= "Failed to parse perform_maintenance",
    testCase "State transition with sell_vehicle and valid input" $
      let initialState = Lib2.emptyState { Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)] }
      in case Lib2.parseQuery "sell_vehicle Car ModelX 2020 25000.0" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.vehicles newState @?= []
            Left err -> error err
        Left err -> error err,
    testCase "State transition with sell_vehicle and invalid price" $
      let initialState = Lib2.emptyState { Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)] }
      in case Lib2.parseQuery "sell_vehicle Car ModelX 2020 twentyfive" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Failed to parse sell_vehicle"
            Right _ -> error "Expected error for invalid price"
        Left err -> err @?= "Failed to parse sell_vehicle",
    testCase "State transition with inventory and valid input" $
      let initialState = Lib2.emptyState { Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000), (Lib2.Car, "ModelY", 2021, 10000)] }
      in case Lib2.parseQuery "inventory Car" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (msg, _) ->
              msg @?= Just "Inventory for Car:\nModelX (2020)\nModelY (2021)\n"
            Left err -> error err
        Left err -> error err,
    testCase "State transition with inventory and invalid vehicle type" $
      let initialState = Lib2.emptyState { Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)] }
      in case Lib2.parseQuery "inventory Plane" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Failed to parse inventory"
            Right _ -> error "Expected error for invalid vehicle type"
        Left err -> err @?= "Failed to parse inventory",
    testCase "State transition with sell_vehicle and vehicle not in inventory" $
      let initialState = Lib2.emptyState { Lib2.vehicles = [(Lib2.Car, "ModelX", 2020, 15000)] }
      in case Lib2.parseQuery "sell_vehicle Car ModelY 2020 25000.0" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Vehicle not found in inventory"
            Right _ -> error "Expected error for vehicle not in inventory"
        Left err -> error err
  ]