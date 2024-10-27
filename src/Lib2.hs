{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
    ( 
    Query(..),
    State(..),
    ID(..),
    Guest(..),
    Hotel(..),
    Floor(..),
    Room(..),
    Amenity(..),
    Date(..),    
    Time(..),
    CheckIn(..),
    CheckOut(..),
    Price(..),
    parseQuery,
    emptyState,
    stateTransition,

    ) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, tails, findIndex, intercalate)


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query =
  Add Hotel|
  Remove ID |
  MakeReservation Guest Hotel CheckIn CheckOut Price  |
  CancelReservation ID |
  AddAdditionalGuest Guest ID |
  ListState

newtype ID = ID Int
  deriving (Show, Eq)

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

instance Show Query where
  show _ = ""
  
-- After all query entries are converted into seperate lines, parse them as seperate inputs
parseLine :: Parser String
parseLine input =
  case lines input of
    (line:rest) -> Right (line, unlines rest)
    [] -> Left "Expected a dot (end of query operation), but got end of input."

-- | Keyword parcing
-- Utility function to check if a line starts with a specific keyword
startsWith :: String -> String -> Bool
startsWith keyword line = keyword `isPrefixOf` line

-- Utility function to parse a specific keyword
parseKeyword :: String -> Parser String
parseKeyword keyword input =
  case parseLine input of
    Right (line, rest) ->
      if startsWith keyword line
        then Right (line, rest)
        else Left $ "Expected keyword: " ++ keyword
    Left err -> Left err

-- | Helper functions for multiple parsers
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
-- Is pat pradziu buvau padares su Parser, todel kad visko taip stipriai neperrasyti tiesiog pasiimti query is tuple (query, string)
parseQuery :: String -> Either String Query
parseQuery input = case splitOn ". " input of
  ("ADD":rest) -> fmap fst (parseAdd (unlines rest))
  ("REMOVE":rest) -> fmap fst (parseRemove (unlines rest))
  ("MAKE RESERVATION":rest) -> fmap fst (parseMakeReservation (unlines rest))
  ("CANCEL RESERVATION":rest) -> fmap fst (parseCancelReservation (unlines rest))
  ("ADD ADDITIONAL GUEST":rest) -> fmap fst (parseAddAdditionalGuest (unlines rest))
  ("LIST":_) -> Right ListState
  _ -> Left "Invalid command"

parseInt :: Parser Int
parseInt input =
  let (numberStr, remaining) = span isDigit input
  in if null numberStr
       then Left "Expected an integer"
       else Right (read numberStr, remaining)

parseID :: Parser ID
parseID input = 
  case parseInt input of
    Right (intValue, remaining) -> Right (ID intValue, remaining)
    Left err -> Left err

-- <add> ::= "ADD. " <hotelsID> 
parseAdd :: Parser Query
parseAdd input =
  case parseHotel input of
    Right (hotel, remaining) -> Right (Add hotel, remaining)
    Left err -> Left err

-- <remove> ::= "REMOVE. " <hotelsID>
parseRemove :: Parser Query
parseRemove input =
  case parseID input of
    Right (id, remaining) -> Right (Remove id, remaining)
    Left err -> Left err

-- <make_reservation> ::= "MAKE RESERVATION. " <guest> <hotel> <check_in> <check_out> <price>
parseMakeReservation :: Parser Query
parseMakeReservation =
  and5' MakeReservation parseGuest parseHotel parseCheckIn parseCheckOut parsePrice


-- <cancel_reservation> ::= "CANCEL RESERVATION. " <reservationID>
parseCancelReservation :: Parser Query
parseCancelReservation input =
  case parseID input of
    Right (id, remaining) -> Right (CancelReservation id, remaining)
    Left err -> Left err

-- <add_additional_guest> ::= "ADD ADDITIONAL GUEST. " <guest> <reservationID>
parseAddAdditionalGuest :: Parser Query
parseAddAdditionalGuest =
  and2' AddAdditionalGuest parseGuest parseID

-- | Parsing the hotel.
-- <hotel> ::= "HOTEL: " <text> ". " |  <hotel> "CHAIN OF " <hotel> | <hotel> <floors>
parseHotel :: Parser Hotel
parseHotel = and3' Hotel parseHotelName parseHotelChain parseFloors

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

-- | Parsing floors.
-- <floors> ::= <floor> | <floor> <floors>
parseFloors :: Parser [Floor]
parseFloors input =
  case parseFloor input of
    Right (floor, remaining) ->
      case parseFloors remaining of
        Right (moreFloors, finalRest) ->
          Right (floor : moreFloors, finalRest)
        Left _ -> Right ([floor], remaining)
    Left _ -> Right ([], input)

-- <floor> ::= "FLOOR: " <number> ". " <rooms>
parseFloor :: Parser Floor
parseFloor input = 
  case parseKeyword "FLOOR: " input of
    Right (line, rest) ->
      let floorNum = read (drop 7 line) :: Int -- removing floor prefix and reading the number
      in case parseRooms rest of
           Right (roomsList, finalRest) -> Right (Floor floorNum roomsList, finalRest)
           Left err -> Left err
    Left err -> Left err

-- | Parsing rooms.

-- <rooms> :: <room> | <room> <rooms>
parseRooms :: Parser [Room]
parseRooms input =
  case parseRoom input of
    Right (room, remaining) ->
      case parseRooms remaining of
        Right (moreRooms, finalRest) ->
          Right (room : moreRooms, finalRest)
        Left _ -> Right ([room], remaining)
    Left _ -> Right ([], input)

-- <room> ::= "ROOM: " <number> ". " | <room> "ROOM SECTION " <room> | <room> <amenities> ". "
parseRoom :: Parser Room
parseRoom input =
  case parseKeyword "ROOM: " input of
    Right (line, rest) ->
      let roomNum = read (drop 6 line) :: Int
      in case parseAmenities rest of  
           Right (amenitiesList, remaining) ->
             case parseRoomSections remaining of  
               Right (sections, finalRest) ->
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

-- | Parsing amenities.

-- <amenities> ::= "AMENITIES: " <amenity> | <amenities> ", " <amenity>
parseAmenities :: Parser [Amenity]
parseAmenities input =
  case parseKeyword "AMENITIES: " input of
    Right (line, rest) ->
      let amenitiesListStr = drop 11 line
          amenitiesList = splitAmenities [amenitiesListStr]
      in Right (amenitiesList, rest)
    Left _ -> Right ([], input)


splitAmenities :: [String] -> [Amenity]
splitAmenities [] = []
splitAmenities input =
  let amenitiesStr = concat input
      amenitiesWords = splitOn ", " amenitiesStr
  in parseAmenitiesList amenitiesWords

parseAmenitiesList :: [String] -> [Amenity]
parseAmenitiesList [] = []
parseAmenitiesList (word:rest) =
  let amenity = parseAmenity word
  in amenity : parseAmenitiesList rest

-- <amenity> ::= "TV" | "WI-FI" | "MINI-BAR" | "BALCONY" | "AC"
parseAmenity :: String -> Amenity
parseAmenity str = case str of
  "TV" -> TV
  "WiFi" -> WiFi
  "Minibar" -> Minibar
  "Balcony" -> Balcony
  "AC" -> AC
  _ -> Unknown

-- <guest> ::= "GUEST: " <name> " " <surname> ". "
parseGuest :: Parser Guest
parseGuest input =
  case parseKeyword "GUEST: " input of
    Right (line, rest) ->
      let names = words (drop (length "GUEST: ") line)
      in case names of
        [name, surname] -> Right (Guest name surname, rest)
        _ -> Left "Invalid guest format."
    Left _ -> Left "Invalid guest format."

-- <check_in> ::= "CHECK IN: " <date> " " <time> ". "
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

-- <check_out> ::= "CHECK OUT: " <date> " " <time> ". "
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

-- <date> ::= <digit> <digit> <digit> <digit> "-" <digit> <digit> "-" <digit> <digit>
parseDate :: Parser Date
parseDate input =
  let (dateStr, rest) = break isSpace input
      dateParts = splitOn "-" dateStr
  in case dateParts of
       [yearStr, monthStr, dayStr] ->
         let yearParsed = read yearStr :: Int
             monthParsed = read monthStr :: Int
             dayParsed = read dayStr :: Int
         in Right (Date yearParsed monthParsed dayParsed, dropWhile isSpace rest)
       _ -> Left "Invalid date format."

-- <time> ::= <digit> <digit> ":" <digit> <digit>
parseTime :: Parser Time
parseTime input =
  let (timeStr, rest) = break isSpace input
      timeParts = splitOn ":" timeStr
  in case timeParts of
       [hourStr, minuteStr] ->
         let hourParsed = read hourStr :: Int
             minuteParsed = read minuteStr :: Int
         in Right (Time hourParsed minuteParsed, dropWhile isSpace rest)
       _ -> Left "Invalid time format."


-- | Function to split strings based on another string.

splitOn :: String -> String -> [String]
splitOn _ [] = []
splitOn delimiter str =
  let (before, remainder) = breakOn delimiter str
  in before : if null remainder
     then []
     else splitOn delimiter (drop (length delimiter) remainder)

-- Helper function to find the first occurrence of the delimiter
breakOn :: String -> String -> (String, String)
breakOn delim s =
  case findIndex (isPrefixOf delim) (tails s) of
    Just idx -> splitAt idx s
    Nothing  -> (s, "")

-- <price> ::= "PRICE: " <number> ". "
parsePrice :: Parser Price
parsePrice input = 
  case parseKeyword "PRICE: " input of
    Right (line, rest) ->
      let number = read (drop 7 line) :: Int 
      in Right (Price number, rest)
    Left err -> Left err




-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Reservation = Reservation {
  reservationID :: ID,
  hotel :: Hotel,
  guests :: [Guest],
  checkIn :: CheckIn,
  checkOut :: CheckOut,
  price :: Price
} deriving (Show, Eq)

data AvailableHotelEntity = AvailableHotelEntity {
  availableEntityId :: ID,
  availableHotel :: Hotel
} deriving (Show, Eq)

data State = State {
  reservations :: [Reservation],
  availableHotelEntities :: [AvailableHotelEntity]
} deriving (Show, Eq)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {
  reservations = [],
  availableHotelEntities = []
}

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  Add hotel ->
    let newId = ID (length (availableHotelEntities st) + 1)
        newHotelEntity = AvailableHotelEntity newId hotel
        newHotels = newHotelEntity : availableHotelEntities st
        newState = st { availableHotelEntities = newHotels }
    in Right (Just $ "Hotel added successfully! ", newState)

  Remove (ID entityId) ->
    let newHotelEntities = filter (\h -> availableEntityId h /= ID entityId) (availableHotelEntities st)
        newState = st { availableHotelEntities = newHotelEntities }
    in Right (Just "Hotel removed successfully!", newState)

  MakeReservation guest hotel checkIn checkOut price ->
    -- Check if the hotel exists in availableHotelEntities
    if any (\h -> availableHotel h == hotel) (availableHotelEntities st) then
      let newId = ID (length (reservations st) + 1)
          newReservation = Reservation newId hotel [guest] checkIn checkOut price
          newReservations = newReservation : reservations st
          newState = st { reservations = newReservations }
      in Right (Just $ "Reservation made successfully!", newState)
    else
      Left "Error: Hotel does not exist."

  CancelReservation (ID resID) ->
    let newReservations = filter (\r -> reservationID r /= ID resID) (reservations st)
        newState = st { reservations = newReservations }
    in Right (Just "Reservation cancelled successfully!", newState)

  AddAdditionalGuest guest (ID reservID) ->
    case filter (\r -> reservationID r == ID reservID) (reservations st) of
      [] -> Left "Error: Reservation not found."
      (reservation:_) ->
        let updatedReservation = reservation { guests = guest : guests reservation }
            newReservations = updatedReservation : filter (\r -> reservationID r /= ID reservID) (reservations st)
            newState = st { reservations = newReservations }
        in Right (Just "Guest added successfully!", newState)

  ListState ->
    let reservationsList = map formatReservation (reservations st) -- converting reservations to string format
        hotelsList = map formatHotel (availableHotelEntities st)
        result = "Reservations: \n" ++ unlines reservationsList ++
                  "\nAvailable hotels/hotel rooms:\n" ++ unlines hotelsList
    in Right (Just result, st)

-- | Formatiing for the ListState.
formatHotel :: AvailableHotelEntity -> String
formatHotel (AvailableHotelEntity (ID id) hotel) =
  "Hotel ID: " ++ show id ++ "\n" ++ formatHotelDetails hotel

formatHotelDetails :: Hotel -> String
formatHotelDetails (Hotel name chain floors) =
  "Hotel Name: " ++ name ++ "\n" ++
  (if null chain
    then "Hotel Chain: None\n"
    else "Hotel Chain: " ++ unlines (map formatHotelDetails chain)) ++
  "Floors:\n" ++ unlines (map formatFloor floors)

formatFloor :: Floor -> String
formatFloor (Floor number rooms) =
  "  Floor Number: " ++ show number ++ "\n" ++
  "  Rooms:\n" ++ unlines (map formatRoom rooms)

formatRoom :: Room -> String
formatRoom (Room number sections amenities) =
  "    Room Number: " ++ show number ++ "\n" ++
  (if null amenities
     then "    Amenities: None\n"
     else "    Amenities: " ++ unwords (map show amenities) ++ "\n") ++
  (if null sections
     then "    Sections: None\n" 
     else "\n    Sections: \n" ++ unlines (map formatRoom sections))

formatReservation :: Reservation -> String
formatReservation (Reservation (ID id) hotel guests checkIn checkOut price) =
  "Reservation ID: " ++ show id ++ "\n" ++
  formatHotelDetails hotel ++
  "Guests:\n" ++ unlines (map formatGuest guests) ++
  "Check-in: " ++ formatCheckIn checkIn ++ "\n" ++
  "Check-out: " ++ formatCheckOut checkOut ++ "\n" ++
  "Price: " ++ formatPrice price ++ " euros \n"

formatGuest :: Guest -> String
formatGuest (Guest name surname) = 
  "Guest: " ++ name ++ " " ++ surname

formatCheckIn :: CheckIn -> String
formatCheckIn (CheckIn date time) =
  "Date: " ++ formatDate date ++ ", Time: " ++ formatTime time

formatCheckOut :: CheckOut -> String
formatCheckOut (CheckOut date time) =
  "Date: " ++ formatDate date ++ ", Time: " ++ formatTime time

formatDate :: Date -> String
formatDate (Date year month day) =
  show year ++ "-" ++ show month ++ "-" ++ show day

formatTime :: Time -> String
formatTime (Time hour minute) =
  show hour ++ ":" ++ show minute

formatPrice :: Price -> String
formatPrice (Price number) =
  show number
  

