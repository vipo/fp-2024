{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
  ( Query(..),
    VehicleType(..),
    MaintenanceType(..),
    Duration(..),
    parseTask,
    parseTaskList,
    skipSpaces,
    parseLiteral,
    parseChar,
    parseString,
    parseInt,
    char,
    Parser (..)
  )
where

import Control.Applicative (Alternative (empty), (<|>), optional)
import Data.Char (isDigit)

-- Data Types for Queries and State
data Query
  = AddVehicle VehicleType String Int Int -- VehicleType, Model, Year, Mileage
  | PerformMaintenance VehicleType MaintenanceType Duration -- VehicleType, MaintenanceType, Duration
  | SellVehicle VehicleType String Int Double -- VehicleType, Model, Year, Price
  | Inventory VehicleType -- VehicleType
  | View
  | Sequence [Query] -- Sequence of queries
  deriving (Eq, Show)

data VehicleType = Car | Truck | Motorcycle | SUV
  deriving (Eq, Show)

data MaintenanceType = OilChange | TireRotation | BrakeInspection | EngineTuneUp
  deriving (Eq, Show)

data Duration = Hours Int | Days Int
  deriving (Eq, Show)

newtype Parser a = P { parse :: String -> Either String (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

-- <task_list> ::= <task> | <task> ";" <task_list>
parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

-- <task> ::= <add_vehicle> | <perform_maintenance> | <sell_vehicle> | <inventory> | <view>
parseTask :: Parser Query
parseTask =
  parseAddVehicle <|>
  parsePerformMaintenance <|>
  parseSellVehicle <|>
  parseInventory <|>
  parseView <|>
  parseVehicleGarage

-- <add_vehicle> ::= "add_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <mileage> ")"
parseAddVehicle :: Parser Query
parseAddVehicle = do
  _ <- parseLiteral "add_vehicle"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  model <- parseString
  _ <- parseChar ','
  year <- parseInt
  _ <- parseChar ','
  mileage <- parseMileage
  _ <- parseChar ')'
  return $ AddVehicle vType model year mileage

parsePerformMaintenance :: Parser Query
parsePerformMaintenance = do
  _ <- parseLiteral "perform_maintenance"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  mType <- parseMaintenanceType
  _ <- parseChar ','
  duration <- parseDuration
  _ <- parseChar ')'
  return $ PerformMaintenance vType mType duration

-- <sell_vehicle> ::= "sell_vehicle" "(" <vehicle_type> "," <model> "," <year> "," <price> ")"
parseSellVehicle :: Parser Query
parseSellVehicle = do
  _ <- parseLiteral "sell_vehicle"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ','
  model <- parseString
  _ <- parseChar ','
  year <- parseInt
  _ <- parseChar ','
  price <- parsePrice
  _ <- parseChar ')'
  return $ SellVehicle vType model year price

-- <inventory> ::= "inventory" "(" <vehicle_type> ")"
parseInventory :: Parser Query
parseInventory = do
  _ <- parseLiteral "inventory"
  _ <- parseChar '('
  vType <- parseVehicleType
  _ <- parseChar ')'
  return $ Inventory vType

-- <view> ::= "view" "(" ")"
parseView :: Parser Query
parseView = do
  _ <- parseLiteral "view"
  _ <- parseChar '('
  _ <- parseChar ')'
  return View

-- <vehicle_garage> ::= "vehicle_garage" "(" <task_list> ")"
parseVehicleGarage :: Parser Query
parseVehicleGarage = do
  _ <- parseLiteral "vehicle_garage"
  _ <- parseChar '('
  queryList <- parseTaskList
  _ <- parseChar ')'
  return $ Sequence queryList

-- <vehicle_type> ::= "Car" | "Truck" | "Motorcycle" | "SUV"
parseVehicleType :: Parser VehicleType
parseVehicleType =
  (parseLiteral "Car" >> return Car) <|>
  (parseLiteral "Truck" >> return Truck) <|>
  (parseLiteral "Motorcycle" >> return Motorcycle) <|>
  (parseLiteral "SUV" >> return SUV)

-- <mileage> parser for mileage input (e.g., "123km")
parseMileage :: Parser Int
parseMileage = do
  num <- parseInt
  _ <- parseLiteral "km"
  return num

-- <maintenance_type> ::= "OilChange" | "TireRotation" | "BrakeInspection" | "EngineTuneUp"
parseMaintenanceType :: Parser MaintenanceType
parseMaintenanceType =
  (parseLiteral "OilChange" >> return OilChange) <|>
  (parseLiteral "TireRotation" >> return TireRotation) <|>
  (parseLiteral "BrakeInspection" >> return BrakeInspection) <|>
  (parseLiteral "EngineTuneUp" >> return EngineTuneUp)

-- <duration> ::= <number> "hours" | <number> "days"
parseDuration :: Parser Duration
parseDuration = do
  num <- parseInt
  unit <- parseDurationUnit
  return (unit num)

-- Helper function to parse duration units
parseDurationUnit :: Parser (Int -> Duration)
parseDurationUnit = do
  unit <- parseString
  case unit of
    "days" -> return Days
    "hours" -> return Hours
    _ -> empty

-- <price> ::= <number> "." <number>
parsePrice :: Parser Double
parsePrice = do
  wholePart <- parseInt
  _ <- parseChar '.'
  fractionalPart <- parseInt
  return (read (show wholePart ++ "." ++ show fractionalPart))

sat :: (Char -> Bool) -> Parser Char
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = P $ \input ->
  let input' = dropWhile (== ' ') input
   in Right ((), input')

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = P $ \input ->
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

parseInt :: Parser Int
parseInt = P $ \input ->
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)