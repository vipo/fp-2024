{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Concurrent (forkIO, newChan)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO)
import Data.Char (isAlpha)
import Data.List
import Data.Ord
import Lib2 qualified
import Lib3 qualified
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf1, suchThat)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

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
          @?= Right (Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000),
      testCase "PerformMaintenance command parsing" $
        Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2 hours)"
          @?= Right (Lib2.PerformMaintenance Lib2.Truck Lib2.OilChange (Lib2.Hours 2)),
      testCase "SellVehicle command parsing" $
        Lib2.parseQuery "sell_vehicle(SUV, \"ModelY\", 2018, 25000.50)"
          @?= Right (Lib2.SellVehicle Lib2.SUV "ModelY" 2018 25000.50),
      testCase "Inventory command parsing" $
        Lib2.parseQuery "inventory(Motorcycle)"
          @?= Right (Lib2.Inventory Lib2.Motorcycle),
      testCase "View command parsing" $
        Lib2.parseQuery "view()"
          @?= Right Lib2.View,
      testCase "Invalid command parsing" $
        Lib2.parseQuery "invalid command"
          @?= Left "Could not recognize: invalid command",
      testCase "State transition with AddVehicle" $ do
        let initialState = Lib2.emptyState
        case Lib2.parseQuery "add_vehicle(Car, \"ModelX\", 2020, 15000km)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "Added Car ModelX (2020)"
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
                msg @?= "Sold Car ModelX (2020) for $25000.5"
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
        case Lib2.parseQuery "perform_maintenance(Truck, OilChange, 2 hours)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, newState) -> do
                msg @?= "Performed OilChange on Truck for Hours 2"
                newState @?= initialState
              Left err -> error err
          Left err -> error err,
      testCase "State transition with Inventory" $ do
        let initialState = Lib2.emptyState {Lib2.vehicles = [(Lib2.Motorcycle, "ModelZ", 2019, 5000)]}
        case Lib2.parseQuery "inventory(Motorcycle)" of
          Right query ->
            case Lib2.stateTransition initialState query of
              Right (Just msg, _) -> msg @?= "Inventory for Motorcycle:\nModelZ (2019)\n"
              Left err -> error err
          Left err -> error err,
      testCase "State transition with Sequence" $ do
        let initialState = Lib2.emptyState
            queries = [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000, Lib2.AddVehicle Lib2.Truck "ModelY" 2019 20000]
        case Lib2.stateTransition initialState (Lib2.Sequence queries) of
          Right (Just msg, newState) -> do
            msg @?= "\nAdded Car ModelX (2020)\nAdded Truck ModelY (2019)"
            Lib2.vehicles newState @?= [(Lib2.Truck, "ModelY", 2019, 20000), (Lib2.Car, "ModelX", 2020, 15000)]
          Left err -> error err,
      testCase "SaveCommand state transition" $ do
        initialState <- newTVarIO Lib2.emptyState
        ioChan <- newChan
        _ <- forkIO $ Lib3.storageOpLoop ioChan
        result <- Lib3.stateTransition initialState Lib3.SaveCommand ioChan
        result @?= Right (Just "State saved successfully"),
      testCase "Batch processing with valid queries" $ do
        initialState <- newTVarIO Lib2.emptyState
        let queries = Lib3.Batch [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000, Lib2.AddVehicle Lib2.Truck "ModelY" 2019 20000]
        result <- atomically $ Lib3.atomicStatements initialState queries
        case result of
          Right msg -> do
            msg @?= Just "Added Car ModelX (2020)\nAdded Truck ModelY (2019)"
            newState <- readTVarIO initialState
            Lib2.vehicles newState @?= [(Lib2.Truck, "ModelY", 2019, 20000), (Lib2.Car, "ModelX", 2020, 15000)]
          Left err -> error err,
      testCase "Batch processing with invalid query" $ do
        initialState <- newTVarIO Lib2.emptyState
        let queries = Lib3.Batch [Lib2.AddVehicle Lib2.Car "ModelX" 2020 15000, Lib2.SellVehicle Lib2.Truck "ModelY" 2019 20000]
        result <- atomically $ Lib3.atomicStatements initialState queries
        result @?= Left "Vehicle not found in inventory"
    ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> arbitrary]

instance Arbitrary Lib2.Query where
  arbitrary =
    oneof
      [ Lib2.AddVehicle <$> arbitrary <*> nonEmptyAlphabeticString <*> positiveInt <*> positiveInt,
        Lib2.PerformMaintenance <$> arbitrary <*> arbitrary <*> arbitrary,
        Lib2.SellVehicle <$> arbitrary <*> nonEmptyAlphabeticString <*> positiveInt <*> positiveDouble,
        Lib2.Inventory <$> arbitrary,
        pure Lib2.View
      ]

nonEmptyAlphabeticString :: Gen String
nonEmptyAlphabeticString = listOf1 (arbitrary `suchThat` isAlpha)

positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 0)

positiveDouble :: Gen Double
positiveDouble = do
  wholePart <- positiveInt
  fractionalPart <- positiveInt
  return (read (show wholePart ++ "." ++ show fractionalPart) :: Double)

instance Arbitrary Lib2.VehicleType where
  arbitrary = elements [Lib2.Car, Lib2.Truck, Lib2.SUV, Lib2.Motorcycle]

instance Arbitrary Lib2.MaintenanceType where
  arbitrary = elements [Lib2.OilChange, Lib2.TireRotation, Lib2.BrakeInspection, Lib2.EngineTuneUp]

instance Arbitrary Lib2.Duration where
  arbitrary = oneof [Lib2.Hours <$> positiveInt, Lib2.Days <$> positiveInt]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ QC.testProperty "parseStatements . renderStatements == Right (statements, \"\")" $
        \statements ->
          Lib3.parseStatements (Lib3.renderStatements statements) == Right (statements, "")
    ]