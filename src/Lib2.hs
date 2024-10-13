{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Lib2
  ( Query (..),
    VehicleType (..),
    MaintenanceType (..),
    Duration (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Data.Char (isDigit)

-- | Data types for Queries and State
data Query
  = AddVehicle VehicleType String Int Int -- VehicleType, Model, Year, Mileage
  | PerformMaintenance VehicleType MaintenanceType Duration -- VehicleType, MaintenanceType, Duration
  | SellVehicle VehicleType String Int Double -- VehicleType, Model, Year, Price
  | Inventory VehicleType -- VehicleType
  | View
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

-- <vehicle_garage> ::= <task_list>
parseVehicleGarage :: Parser [Query]
parseVehicleGarage = parseTaskList

-- <task_list> ::= <task> | <task> ";" <task_list>
parseTaskList :: Parser [Query]
parseTaskList input =
  case parseTask input of
    Right (task, rest) ->
      case parseChar ';' rest of
        Right (_, rest') ->
          case parseTaskList rest' of
            Right (tasks, rest'') -> Right (task : tasks, rest'')
            Left _ -> Right ([task], rest)
        Left _ -> Right ([task], rest)
    Left err -> Left err

-- <task> ::= <add_vehicle> | <perform_maintenance> | <sell_vehicle> | <inventory> | <view>
parseTask :: Parser Query
parseTask = or5' parseAddVehicle parsePerformMaintenance parseSellVehicle parseInventory parseView

-- <add_vehicle> ::= "add_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <mileage> ")"
parseAddVehicle :: Parser Query
parseAddVehicle =
  and10'
    (\_ _ vType _ model _ year _ mileage _ -> AddVehicle vType model year mileage)
    (parseLiteral "add_vehicle")
    (parseChar '(')
    parseVehicleType
    (parseChar ',')
    parseString
    (parseChar ',')
    parseInt
    (parseChar ',')
    parseMileage
    (parseChar ')')

-- <perform_maintenance> ::= "perform_maintenance" "(" <vehicle_type> "," <maintenance_type> "," <duration> ")"
parsePerformMaintenance :: Parser Query
parsePerformMaintenance =
  and8'
    (\_ _ vType _ mType _ duration _ -> PerformMaintenance vType mType duration)
    (parseLiteral "perform_maintenance")
    (parseChar '(')
    parseVehicleType
    (parseChar ',')
    parseMaintenanceType
    (parseChar ',')
    parseDuration
    (parseChar ')')

-- <sell_vehicle> ::= "sell_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <price> ")"
parseSellVehicle :: Parser Query
parseSellVehicle =
  and10'
    (\_ _ vType _ model _ year _ price _ -> SellVehicle vType model year price)
    (parseLiteral "sell_vehicle")
    (parseChar '(')
    parseVehicleType
    (parseChar ',')
    parseString
    (parseChar ',')
    parseInt
    (parseChar ',')
    parsePrice
    (parseChar ')')

-- <inventory> ::= "inventory" "(" <vehicle_type> ")"
parseInventory :: Parser Query
parseInventory =
  and4'
    (\_ _ vType _ -> Inventory vType)
    (parseLiteral "inventory")
    (parseChar '(')
    parseVehicleType
    (parseChar ')')

-- <view> ::= "view" "(" ")"
parseView :: Parser Query
parseView =
  and3'
    (\_ _ _ -> View)
    (parseLiteral "view")
    (parseChar '(')
    (parseChar ')')

-- <vehicle_type> ::= "Car" | "Truck" | "Motorcycle" | "SUV"
parseVehicleType :: Parser VehicleType
parseVehicleType input = case parseString (skipSpaces input) of
  Right ("Car", rest) -> Right (Car, rest)
  Right ("Truck", rest) -> Right (Truck, rest)
  Right ("Motorcycle", rest) -> Right (Motorcycle, rest)
  Right ("SUV", rest) -> Right (SUV, rest)
  _ -> Left "Failed to parse vehicle type"

-- <model> ::= <string>
parseString :: Parser String
parseString input =
  let input' = skipSpaces input
   in if null input'
        then Right ("", "")
        else
          if head input' == '"'
            then parseQuotedString (tail input')
            else
              let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x : str, rest')
      Left err -> Left err

-- <year> ::= <number>
parseInt :: Parser Int
parseInt input =
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

-- <mileage> ::= <number> "km"
parseMileage :: Parser Int
parseMileage input =
  case parseInt input of
    Right (num, rest) -> case parseLiteral "km" rest of
      Right (_, rest') -> Right (num, rest')
      Left err -> Left err
    Left err -> Left err

-- <maintenance_type> ::= "OilChange" | "TireRotation" | "BrakeInspection" | "EngineTuneUp"
parseMaintenanceType :: Parser MaintenanceType
parseMaintenanceType input = case parseString (skipSpaces input) of
  Right ("OilChange", rest) -> Right (OilChange, rest)
  Right ("TireRotation", rest) -> Right (TireRotation, rest)
  Right ("BrakeInspection", rest) -> Right (BrakeInspection, rest)
  Right ("EngineTuneUp", rest) -> Right (EngineTuneUp, rest)
  _ -> Left "Failed to parse maintenance type"

-- <duration> ::= <number> "hours" | <number> "days"
parseDuration :: Parser Duration
parseDuration input = case parseInt input of
  Right (num, rest) -> case parseDurationUnit rest of
    Right (unit, rest') -> Right (unit num, rest')
    Left err -> Left err
  Left err -> Left err

-- Helper function to parse duration unit
parseDurationUnit :: Parser (Int -> Duration)
parseDurationUnit input = case parseString (skipSpaces input) of
  Right ("days", rest) -> Right (Days, rest)
  Right ("hours", rest) -> Right (Hours, rest)
  _ -> Left "Failed to parse duration unit"

-- <price> ::= <number> "." <number>
parsePrice :: Parser Double
parsePrice input =
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected a number"
        else case rest of
          ('.' : rest') ->
            let (fraction, rest'') = span isDigit rest'
             in if null fraction
                  then Left "Expected digits after decimal point"
                  else Right (read (digits ++ "." ++ fraction), rest'')
          _ -> Left "Expected '.' for price"

and3' :: (a1 -> a2 -> a3 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser d
and3' f a1 a2 a3 = \input ->
  case a1 input of
    Right (v1, r1) ->
      case a2 r1 of
        Right (v2, r2) ->
          case a3 r2 of
            Right (v3, r3) -> Right (f v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and4' :: (a1 -> a2 -> a3 -> a4 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser a4 -> Parser d
and4' f a1 a2 a3 a4 = \input ->
  case a1 input of
    Right (v1, r1) ->
      case a2 r1 of
        Right (v2, r2) ->
          case a3 r2 of
            Right (v3, r3) ->
              case a4 r3 of
                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and8' :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser a4 -> Parser a5 -> Parser a6 -> Parser a7 -> Parser a8 -> Parser d
and8' f a1 a2 a3 a4 a5 a6 a7 a8 = \input ->
  case a1 input of
    Right (v1, r1) ->
      case a2 r1 of
        Right (v2, r2) ->
          case a3 r2 of
            Right (v3, r3) ->
              case a4 r3 of
                Right (v4, r4) ->
                  case a5 r4 of
                    Right (v5, r5) ->
                      case a6 r5 of
                        Right (v6, r6) ->
                          case a7 r6 of
                            Right (v7, r7) ->
                              case a8 r7 of
                                Right (v8, r8) -> Right (f v1 v2 v3 v4 v5 v6 v7 v8, r8)
                                Left e8 -> Left e8
                            Left e7 -> Left e7
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and10' :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser a4 -> Parser a5 -> Parser a6 -> Parser a7 -> Parser a8 -> Parser a9 -> Parser a10 -> Parser d
and10' f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = \input ->
  case a1 input of
    Right (v1, r1) ->
      case a2 r1 of
        Right (v2, r2) ->
          case a3 r2 of
            Right (v3, r3) ->
              case a4 r3 of
                Right (v4, r4) ->
                  case a5 r4 of
                    Right (v5, r5) ->
                      case a6 r5 of
                        Right (v6, r6) ->
                          case a7 r6 of
                            Right (v7, r7) ->
                              case a8 r7 of
                                Right (v8, r8) ->
                                  case a9 r8 of
                                    Right (v9, r9) ->
                                      case a10 r9 of
                                        Right (v10, r10) -> Right (f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10, r10)
                                        Left e10 -> Left e10
                                    Left e9 -> Left e9
                                Left e8 -> Left e8
                            Left e7 -> Left e7
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' p1 p2 p3 p4 p5 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left e1 -> case p2 input of
      Right r2 -> Right r2
      Left e2 -> case p3 input of
        Right r3 -> Right r3
        Left e3 -> case p4 input of
          Right r4 -> Right r4
          Left e4 -> case p5 input of
            Right r5 -> Right r5
            Left e5 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5)

-- | Initial program state.
emptyState :: State
emptyState = State {vehicles = [], inventory = []}

-- | Basic parsers
type Parser a = String -> Either String (a, String)

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x : xs) input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == x
            then case parseLiteral xs (tail input') of
              Right (str, rest) -> Right (x : str, rest)
              Left err -> Left err
            else Left $ "Expected \"" ++ (x : xs) ++ "\", but found \"" ++ take (length (x : xs)) input' ++ "\""

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == c
            then Right (c, tail input')
            else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

parseQuery :: String -> Either String [Query]
parseQuery input =
  case parseVehicleGarage input of
    Right (queries, _) -> Right queries
    Left err -> Left $ "Failed to parse vehicle garage: " ++ err

-- | Transition the state with a list of queries.
stateTransition :: State -> [Query] -> Either String (Maybe String, State)
stateTransition st [] = Right (Nothing, st)
stateTransition st (q : qs) =
  case singleStateTransition st q of
    Right (msg, newState) ->
      case stateTransition newState qs of
        Right (msgs, finalState) -> Right (combineMessages msg msgs, finalState)
        Left err -> Left err
    Left err -> Left err

-- | Helper function to transition the state with a single query.
singleStateTransition :: State -> Query -> Either String (Maybe String, State)
singleStateTransition st query = case query of
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

-- | Helper function to combine messages
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