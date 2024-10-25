{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query =
  Add Hotel|
  Remove Hotel |
  MakeReservation Guest Hotel CheckIn CheckOut Price  |
  CancelReservation Hotel |
  AddAdditionalGuest Guest Hotel
  deriving (Show)


data Guest = Guest{
  guestName :: String,
  guestSurname :: String 
} deriving (Show, Eq)

data Hotel = Hotel {
  hotelName :: String,
  hotelChain :: [Hotel], -- possible that the hotel may not have a chain (empty)
  floors :: [Floor]
} deriving (Show, Eq)

data Floor = Floor {
  floorNumber :: Int,
  rooms :: [Room] -- floor can have many rooms attached to it
} deriving (Show, Eq)

data Room = Room {
  roomNumber :: Int,
  roomSections :: [Room], -- empty list if no room sections
  amenities :: [Amenity]
} deriving (Show, Eq)

data Amenity = TV | WiFi | Minibar | Balcony | AC | Unknown
  deriving (Show, Eq)

data Date = Date { year :: Int, month :: Int, day :: Int} deriving (Show, Eq)

data Time = Time {
  hour :: Int,
  minute :: Int
} deriving (Show, Eq)

data CheckIn = CheckIn {
  checkInDate :: Date,
  checkOuttime :: Time
} deriving (Show, Eq)

data CheckOut = CheckOut {
  checkOutDate :: Date,
  checkOutTime :: Time
} deriving (Show, Eq)

data Price = Price Int
  deriving (Show, Eq)


-- | Parser type
type Parser a = String -> Either String (a, String)


-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

--instance Show Query where
  --show _ = ""
  
parseLine :: Parser String
parseLine input =
  case lines input of
    (line:rest) -> Right (line, unlines rest)
    [] -> Left "Expected a line, but got end of input."

-- utility function to check if a line starts with a specific keyword
startsWith :: String -> String -> Bool
startsWith keyword line = keyword `isPrefixOf` line

-- utility function to parse a specific keyword
parseKeyword :: String -> Parser String
parseKeyword keyword input =
  case parseLine input of
    Right (line, rest) ->
      if startsWith keyword line
        then Right (line, rest)
        else Left $ "Expected keyword: " ++ keyword
    Left err -> Left err



-- parseKeyword :: String -> Parser ()
-- parseKeyword keyword = \input ->
--   case input of
--     (line:rest) | line == keyword -> Right ((), rest)
--     _ -> Left $ "Expected keyword: " ++ keyword



-- parseKeyword' :: String -> [String] -> Either String ([String], [String])
-- parseKeyword' keyword input =
--   if take (length keyword) input == [keyword]
--   then Right ([], drop 1 input) -- consume the keyword line
--   else Left $ "Expected keyword: " ++ keyword

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (c v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' d a b c input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (d v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f 
and5' f a b c d e input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) ->
              case d r3 of 
                Right (v4, r4) ->
                  case e r4 of
                    Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1




-- | Parses user's input.
-- The function must have tests.
parseQuery :: Parser Query
parseQuery input = case lines input of
  ("ADD":rest) -> parseAdd (unlines rest)
  ("REMOVE":rest) -> parseRemove (unlines rest)
  ("MAKE RESERVATION":rest) -> parseMakeReservation (unlines rest)
  ("CANCEL RESERVATION":rest) -> parseCancelReservation (unlines rest)
  ("ADD ADDITIONAL GUEST":rest) -> parseAddAdditionalGuest (unlines rest)
  _ -> Left "Invalid command"

parseAdd :: Parser Query
parseAdd input =
  case parseHotel input of
    Right (hotel, remaining) -> Right (Add hotel, remaining)
    Left err -> Left err

parseRemove :: Parser Query
parseRemove input =
  case parseHotel input of
    Right (hotel, remaining) -> Right (Remove hotel, remaining)
    Left err -> Left err

-- <make_reservation> ::= "MAKE RESERVATION\n" <guest> <hotel> <check_in> <check_out> <price>
parseMakeReservation :: Parser Query
parseMakeReservation =
  and5' MakeReservation parseGuest parseHotel parseCheckIn parseCheckOut parsePrice

parseCancelReservation :: Parser Query
parseCancelReservation input =
  case parseHotel input of
    Right (hotel, remaining) -> Right (CancelReservation hotel, remaining)
    Left err -> Left err

parseAddAdditionalGuest :: Parser Query
parseAddAdditionalGuest =
  and2' AddAdditionalGuest parseGuest parseHotel


parseHotelName :: Parser String
parseHotelName input =
  case parseKeyword "HOTEL: " input of
    Right (line, rest) -> Right (drop 7 line, rest) -- removing hotel prefix
    Left err -> Left err

parseHotelChain :: Parser [Hotel]
parseHotelChain input =
  case parseLine input of
    Right (line, rest) ->
      if startsWith "CHAIN OF" line
        then case parseHotel rest of
          Right (nextHotel, remaining) ->
            case parseHotelChain remaining of
              Right (moreHotels, finalRest) ->
                Right (nextHotel : moreHotels, finalRest)
              Left _ -> Right ([nextHotel], remaining)
          Left _ -> Right ([], input)
      else Right ([], input)
    Left _ -> Right ([], input)


parseFloors :: Parser [Floor]
parseFloors input =
  case parseFloor input of
    Right (floor, remaining) ->
      case parseFloors remaining of
        Right (moreFloors, finalRest) ->
          Right (floor : moreFloors, finalRest)
        Left _ -> Right ([floor], remaining)
    Left _ -> Right ([], input)

parseFloor :: Parser Floor
parseFloor input = 
  case parseKeyword "FLOOR: " input of
    Right (line, rest) ->
      let floorNum = read (drop 7 line) :: Int -- removing floor prefix and reading the number
      in case parseRooms rest of
           Right (roomsList, finalRest) -> Right (Floor floorNum roomsList, finalRest)
           Left err -> Left err
    Left err -> Left err


parseRooms :: Parser [Room]
parseRooms input =
  case parseRoom input of
    Right (room, remaining) ->
      case parseRooms remaining of
        Right (moreRooms, finalRest) ->
          Right (room : moreRooms, finalRest)
        Left _ -> Right ([room], remaining)
    Left _ -> Right ([], input)


parseRoom :: Parser Room
parseRoom input =
  case parseKeyword "ROOM: " input of
    Right (line, rest) ->
      let roomNum = read (drop 6 line) :: Int
      in case parseRoomSections rest of
           Right (sections, remaining) ->
             case parseAmenities remaining of
               Right (amenitiesList, finalRest) ->
                 Right (Room roomNum sections amenitiesList, finalRest)
               Left err -> Left err
           Left err -> Left err
    Left err -> Left err

parseRoomSections :: Parser [Room]
parseRoomSections input =
  case parseKeyword "ROOM SECTION" input of
    Right (_, rest) ->
      case parseRoom rest of
        Right (roomSection, remaining) ->
          case parseRoomSections remaining of
            Right (sections, finalRest) -> Right (roomSection : sections, finalRest)
            Left _ -> Right ([roomSection], remaining)
        Left _ -> Right ([], input)
    Left _ -> Right ([], input)


parseAmenities :: Parser [Amenity]
parseAmenities input =
  case parseKeyword "AMENITIES: " input of
    Right (_, rest) ->
      let amenitiesList = splitAmenities (lines rest)
      in Right (amenitiesList, rest)
    Left _ -> Right ([], input)


splitAmenities :: [String] -> [Amenity]
splitAmenities [] = []
splitAmenities input =
  let amenitiesStr = concat input
      amenitiesWords = splitOnComma amenitiesStr
  in parseAmenitiesList amenitiesWords


parseAmenitiesList :: [String] -> [Amenity]
parseAmenitiesList [] = []
parseAmenitiesList (amenity:rest) =
  case parseAmenity amenity of
    TV -> TV : parseAmenitiesList rest
    WiFi -> WiFi : parseAmenitiesList rest
    Minibar -> Minibar : parseAmenitiesList rest
    Balcony -> Balcony : parseAmenitiesList rest
    AC -> AC : parseAmenitiesList rest
    Unknown -> Unknown : parseAmenitiesList rest

parseAmenity :: String -> Amenity
parseAmenity str = case str of
  "TV" -> TV
  "WiFi" -> WiFi
  "Minibar" -> Minibar
  "Balcony" -> Balcony
  "AC" -> AC
  _ -> Unknown


splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str =
  let (word, rest) = break isComma str
  in word : splitOnComma (dropWhile isSpace (drop 1 rest))
  where isComma c = c == ','



parseGuest :: Parser Guest
parseGuest input =
  case parseKeyword "GUEST: " input of
    Right (line, rest) ->
      let names = words (drop (length "GUEST: ") line)
      in case names of
        [name, surname] -> Right (Guest name surname, rest)
        _ -> Left "Invalid guest format."
    Left _ -> Left "Invalid guest format."


parseCheckIn :: Parser CheckIn
parseCheckIn input =
  case parseKeyword "CHECK IN: " input of
    Right (line, rest) ->
      let parts = words (drop (length "CHECK IN: ") line)
      in case parts of
        [dateString, timeString] ->
          case parseDate dateString of
            Right (date, _) ->
              case parseTime timeString of
                Right (time, _) -> Right (CheckIn date time, rest)
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Invalid check-in format."
    Left _ -> Left "Invalid check-in format."

parseCheckOut :: Parser CheckOut
parseCheckOut input =
  case parseKeyword "CHECK OUT: " input of
    Right (line, rest) ->
      let parts = words (drop (length "CHECK OUT: ") line)
      in case parts of
        [dateString, timeString] ->
          case parseDate dateString of
            Right (date, _) ->
              case parseTime timeString of
                Right (time, _) -> Right (CheckOut date time, rest)
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Invalid check-out format."
    Left _ -> Left "Invalid check-out format."


parseDate :: Parser Date
parseDate input =
  let parts = words input
  in case parts of
       (dateStr:rest) ->
         let dateParts = splitOn (== '-') dateStr
         in case dateParts of
              [yearStr, monthStr, dayStr] ->
                let yearParsed = read yearStr :: Int
                    monthParsed = read monthStr :: Int
                    dayParsed = read dayStr :: Int
                in Right (Date yearParsed monthParsed dayParsed, unwords rest)
              _ -> Left "Invalid date format."
       _ -> Left "Invalid date format."

parseTime :: Parser Time
parseTime input =
  let parts = words input
  in case parts of
       (timeStr:rest) ->
         let timeParts = splitOn (== ':') timeStr
         in case timeParts of
              [hourStr, minuteStr] ->
                let hourParsed = read hourStr :: Int
                    minuteParsed = read minuteStr :: Int
                in Right (Time hourParsed minuteParsed, unwords rest)
              _ -> Left "Invalid time format."
       _ -> Left "Invalid time format."


splitOnSpace :: String -> [String]
splitOnSpace str = splitOn isSpace str

splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn p s =
  let (before, remainder) = break p s
      after = dropWhile p remainder
  in before : splitOn p after



parsePrice :: Parser Price
parsePrice input = 
  case parseKeyword "PRICE: " input of
    Right (line, rest) ->
      let number = read (drop 7 line) :: Int 
      in Right (Price number, rest)
    Left err -> Left err

parseHotel :: Parser Hotel
parseHotel = and3' Hotel parseHotelName parseHotelChain parseFloors


-- <make_reservation> ::= "MAKE RESERVATION\n" <guest> <hotel> <check_in> <check_out> <price>

parseNumber :: Parser Int
parseNumber input = 
  case span isDigit input of
    ("", _) -> Left "Not a number."
    (numberStr, rest) ->
      let number = read numberStr :: Int
      in Right (number, dropWhile isSpace rest)





-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
