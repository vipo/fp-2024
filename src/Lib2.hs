{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
  ( Query (..),
    VehicleType (..),
    MaintenanceType (..),
    Duration (..),
    parseTask,
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Parsers

data State = State
  { vehicles :: [(VehicleType, String, Int, Int)], -- (VehicleType, Model, Year, Mileage)
    inventory :: [(VehicleType, Int)] -- Tracks vehicle inventory by VehicleType
  }
  deriving (Eq, Show)

-- | Initial program state.
emptyState :: State
emptyState = State {vehicles = [], inventory = []}

parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    Left e -> Left e
    Right (qs, r) -> if null r
      then case qs of
        [q] -> Right q
        _ -> Right (Sequence qs)
      else Left ("Unrecognized characters: " ++ r)

-- | Transition the state with a single query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddVehicle vehicleType model year mileage ->
    let updatedVehicles = (vehicleType, model, year, mileage) : vehicles st
        updatedInventory = addToInventory vehicleType 1 (inventory st)
        newState = st {vehicles = updatedVehicles, inventory = updatedInventory}
     in Right (Just ("Added " ++ show vehicleType ++ " " ++ model ++ " (" ++ show year ++ ")"), newState)
  PerformMaintenance vehicleType maintenanceType duration ->
    Right (Just ("Performed " ++ show maintenanceType ++ " on " ++ show vehicleType ++ " for " ++ show duration), st)
  SellVehicle vehicleType model year price ->
    if any (\(v, m, y, _) -> v == vehicleType && m == model && y == year) (vehicles st)
      then
        let newVehicles = filter (\(v, m, y, _) -> not (v == vehicleType && m == model && y == year)) (vehicles st)
            updatedInventory = removeFromInventory vehicleType 1 (inventory st)
            newState = st {vehicles = newVehicles, inventory = updatedInventory}
         in Right (Just ("Sold " ++ show vehicleType ++ " " ++ model ++ " (" ++ show year ++ ") for $" ++ show price), newState)
      else Left "Vehicle not found in inventory"
  Inventory vehicleType ->
    let vehiclesList = filter (\(v, _, _, _) -> v == vehicleType) (vehicles st)
        vehiclesStr = unlines $ map (\(_, m, y, _) -> m ++ " (" ++ show y ++ ")") vehiclesList
     in Right (Just ("Inventory for " ++ show vehicleType ++ ":\n" ++ vehiclesStr), st)
  View ->
    let vehiclesStr = unlines $ map (\(v, m, y, _) -> show v ++ " " ++ m ++ " (" ++ show y ++ ")") (vehicles st)
        inventoryStr = unlines $ map (\(v, q) -> show q ++ " " ++ show v) (inventory st)
     in Right (Just ("Vehicles:\n" ++ vehiclesStr ++ "\nInventory:\n" ++ inventoryStr), st)
  Sequence queryList ->
    foldl processQuery (Right (Just "", st)) queryList
    where
      processQuery :: Either String (Maybe String, State) -> Query -> Either String (Maybe String, State)
      processQuery (Left err) _ = Left err
      processQuery (Right (accMsg, currentState)) nextQuery =
        case stateTransition currentState nextQuery of
          Left err -> Left err
          Right (Just result, newState) ->
            Right (combineMessages accMsg (Just result), newState)
          Right (Nothing, newState) -> Right (accMsg, newState)

-- Helper function to combine messages
combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg) Nothing = Just msg
combineMessages Nothing (Just msg) = Just msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)

-- Helper function to add to inventory
addToInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
addToInventory item quantity [] = [(item, quantity)]
addToInventory item quantity ((i, q) : xs)
  | i == item = (i, q + quantity) : xs
  | otherwise = (i, q) : addToInventory item quantity xs

-- Helper function to remove from inventory
removeFromInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
removeFromInventory _ _ [] = []
removeFromInventory item quantity ((i, q) : xs)
  | i == item = if q > quantity then (i, q - quantity) : xs else xs
  | otherwise = (i, q) : removeFromInventory item quantity xs