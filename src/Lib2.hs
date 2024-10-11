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

import Data.Char (isDigit)

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

-- | Basic parsers
type Parser a = String -> Either String (a, String)

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = skipSpaces input
  in if null input'
     then Left "Unexpected end of input"
     else if head input' == c
          then Right (c, tail input')
          else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x:xs) input =
  let input' = skipSpaces input
  in if null input'
     then Left "Unexpected end of input"
     else if head input' == x
          then case parseLiteral xs (tail input') of
                 Right (str, rest) -> Right (x:str, rest)
                 Left err -> Left err
          else Left $ "Expected \"" ++ (x:xs) ++ "\", but found \"" ++ take (length (x:xs)) input' ++ "\""

parseString :: Parser String
parseString input =
  let input' = skipSpaces input
  in if null input'
     then Right ("", "")
     else if head input' == '"'
          then parseQuotedString (tail input')
          else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"':rest) = Right ("", rest)
    parseQuotedString (x:rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x:str, rest')
      Left err -> Left err

parseInt :: Parser Int
parseInt input =
  let (digits, rest) = span isDigit (skipSpaces input)
  in if null digits
     then Left "Expected an integer"
     else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble input =
  let (digits, rest) = span (\c -> isDigit c || c == '.') (skipSpaces input)
  in if null digits
     then Left "Expected a double"
     else Right (read digits, rest)

-- | Parse vehicle type
parseVehicleType :: Parser VehicleType
parseVehicleType input = case parseString (skipSpaces input) of
  Right ("Car", rest) -> Right (Car, rest)
  Right ("Truck", rest) -> Right (Truck, rest)
  Right ("Motorcycle", rest) -> Right (Motorcycle, rest)
  Right ("SUV", rest) -> Right (SUV, rest)
  _ -> Left "Failed to parse vehicle type"

-- | Parse maintenance type
parseMaintenanceType :: Parser MaintenanceType
parseMaintenanceType input = case parseString (skipSpaces input) of
  Right ("OilChange", rest) -> Right (OilChange, rest)
  Right ("TireRotation", rest) -> Right (TireRotation, rest)
  Right ("BrakeInspection", rest) -> Right (BrakeInspection, rest)
  Right ("EngineTuneUp", rest) -> Right (EngineTuneUp, rest)
  _ -> Left "Failed to parse maintenance type"

-- | Parse duration unit
parseDurationUnit :: Parser (Int -> Duration)
parseDurationUnit input = case parseString (skipSpaces input) of
  Right ("days", rest) -> Right (Days, rest)
  Right ("hours", rest) -> Right (Hours, rest)
  _ -> Left "Failed to parse duration unit"

-- | Parse duration
parseDuration :: Parser Duration
parseDuration input = case parseInt input of
  Right (num, rest) -> case parseDurationUnit rest of
    Right (unit, rest') -> Right (unit num, rest')
    Left err -> Left err
  Left err -> Left err

-- | Parse add vehicle query
parseAddVehicle :: Parser Query
parseAddVehicle input = do
  (_, rest) <- parseLiteral "add_vehicle" input
  (_, rest1) <- parseChar '(' rest
  (vType, rest2) <- parseVehicleType rest1
  (_, rest3) <- parseChar ',' rest2
  (model, rest4) <- parseString rest3
  (_, rest5) <- parseChar ',' rest4
  (year, rest6) <- parseInt rest5
  (_, rest7) <- parseChar ',' rest6
  (mileage, rest8) <- parseInt rest7
  (_, rest9) <- parseChar ')' rest8
  return (AddVehicle vType model year mileage, rest9)

-- | Parse perform maintenance query
parsePerformMaintenance :: Parser Query
parsePerformMaintenance input = do
  (_, rest) <- parseLiteral "perform_maintenance" input
  (_, rest1) <- parseChar '(' rest
  (vType, rest2) <- parseVehicleType rest1
  (_, rest3) <- parseChar ',' rest2
  (mType, rest4) <- parseMaintenanceType rest3
  (_, rest5) <- parseChar ',' rest4
  (duration, rest6) <- parseDuration rest5
  (_, rest7) <- parseChar ')' rest6
  return (PerformMaintenance vType mType duration, rest7)

-- | Parse sell vehicle query
parseSellVehicle :: Parser Query
parseSellVehicle input = do
  (_, rest) <- parseLiteral "sell_vehicle" input
  (_, rest1) <- parseChar '(' rest
  (vType, rest2) <- parseVehicleType rest1
  (_, rest3) <- parseChar ',' rest2
  (model, rest4) <- parseString rest3
  (_, rest5) <- parseChar ',' rest4
  (year, rest6) <- parseInt rest5
  (_, rest7) <- parseChar ',' rest6
  (price, rest8) <- parseDouble rest7
  (_, rest9) <- parseChar ')' rest8
  return (SellVehicle vType model year price, rest9)

-- | Parse inventory query
parseInventory :: Parser Query
parseInventory input = do
  (_, rest) <- parseLiteral "inventory" input
  (_, rest1) <- parseChar '(' rest
  (vType, rest2) <- parseVehicleType rest1
  (_, rest3) <- parseChar ')' rest2
  return (Inventory vType, rest3)

-- | Combine all query parsers
parseQuery :: String -> Either String Query
parseQuery input = case parseAddVehicle input of
  Right (query, _) -> Right query
  Left err1 -> case parsePerformMaintenance input of
    Right (query, _) -> Right query
    Left err2 -> case parseSellVehicle input of
      Right (query, _) -> Right query
      Left err3 -> case parseInventory input of
        Right (query, _) -> Right query
        Left err4 -> Left $ "Failed to parse query: " ++ err1 ++ "; " ++ err2 ++ "; " ++ err3 ++ "; " ++ err4

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