module Lib2
  ( Query (..),
    VehicleType (..),
    MaintenanceType (..),
    Duration (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition
  )
where

import Text.Read (readMaybe)

-- | Data types for Queries and State
data Query
  = AddVehicle VehicleType String Int Int -- VehicleType, Model, Year, Mileage
  | PerformMaintenance VehicleType MaintenanceType Duration -- VehicleType, MaintenanceType, Duration
  | SellVehicle VehicleType String Int Double -- VehicleType, Model, Year, Price
  | Inventory VehicleType
  deriving (Eq, Show)

data VehicleType = Car | Truck | Motorcycle | SUV
  deriving (Eq, Show)

data MaintenanceType = OilChange | TireRotation | BrakeInspection | EngineTuneUp
  deriving (Eq, Show)

data Duration = Hours Int | Days Int
  deriving (Eq, Show)

data State = State
  { vehicles :: [(VehicleType, String, Int, Int)], -- (VehicleType, Model, Year, Mileage)
    inventory :: [(VehicleType, Int)] -- Tracks vehicle inventory by VehicleType
  }
  deriving (Eq, Show)

-- | Initial program state.
emptyState :: State
emptyState = State {vehicles = [], inventory = []}

-- | Parse vehicle type
parseVehicleType :: String -> Maybe VehicleType
parseVehicleType "Car" = Just Car
parseVehicleType "Truck" = Just Truck
parseVehicleType "Motorcycle" = Just Motorcycle
parseVehicleType "SUV" = Just SUV
parseVehicleType _ = Nothing

-- | Parse maintenance type
parseMaintenanceType :: String -> Maybe MaintenanceType
parseMaintenanceType "OilChange" = Just OilChange
parseMaintenanceType "TireRotation" = Just TireRotation
parseMaintenanceType "BrakeInspection" = Just BrakeInspection
parseMaintenanceType "EngineTuneUp" = Just EngineTuneUp
parseMaintenanceType _ = Nothing

-- | Parses user's input into a Query.
parseQuery :: String -> Either String Query
parseQuery input =
  let sanitizedInput = words (map (\c -> if (c == ',') || (c `elem` "()\"") then ' ' else c) input)
   in case sanitizedInput of
        -- <add_vehicle> ::= "add_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <mileage> ")"
        ("add_vehicle" : v : model : year : mileage : _) ->
          case (parseVehicleType v, readMaybe year :: Maybe Int, readMaybe mileage :: Maybe Int) of
            (Just vehicleType, Just y, Just m) -> Right (AddVehicle vehicleType model y m)
            _ -> Left "Failed to parse add_vehicle"
        -- <perform_maintenance> ::= "perform_maintenance" "(" <vehicle_type> "," <maintenance_type> "," <duration_number> "," <duration_unit> ")"
        ("perform_maintenance" : v : m : durationNum : durationUnit : _) ->
          case (parseVehicleType v, parseMaintenanceType m, readMaybe durationNum :: Maybe Int, parseDurationUnit durationUnit) of
            (Just vehicleType, Just maintenanceType, Just num, Just dur) ->
              Right (PerformMaintenance vehicleType maintenanceType (dur num))
            _ -> Left "Failed to parse perform_maintenance"
        -- <sell_vehicle> ::= "sell_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <price> ")"
        ("sell_vehicle" : v : model : year : price : _) ->
          case (parseVehicleType v, readMaybe year :: Maybe Int, readMaybe price :: Maybe Double) of
            (Just vehicleType, Just y, Just p) -> Right (SellVehicle vehicleType model y p)
            _ -> Left "Failed to parse sell_vehicle"
        -- <inventory> ::= "inventory" "(" <vehicle_type> ")"
        ("inventory" : v : _) ->
          case parseVehicleType v of
            Just vehicleType -> Right (Inventory vehicleType)
            _ -> Left "Failed to parse inventory"
        _ -> Left "Unknown command"

-- | Parse duration unit from a string like "days" or "hours".
parseDurationUnit :: String -> Maybe (Int -> Duration)
parseDurationUnit unit =
  case unit of
    "days"  -> Just Days
    "hours" -> Just Hours
    _       -> Nothing

-- | Transition the state with a new query.
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