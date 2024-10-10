{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where


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

data Guest = Guest{
  guestName :: String,
  guestSurname :: String 
} deriving (Show, Eq)

data Hotel = Hotel {
  hotelName :: String,
  hotelChain :: String, -- possible that the hotel may not have a chain (empty)
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

data Amenity = TV | WiFi | Minibar | Balcony | AC 
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

-- commands


-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input = case lines input of
  ("ADD\n":rest) -> parseAdd rest
  ("REMOVE\n":rest) -> parseRemoveHotelRoom rest
  ("MAKE RESERVATION\n":rest) -> parseMakeReservation rest
  ("CANCEL RESERVATION\n":rest) -> parseCancelReservation rest
  ("ADD ADDITIONAL GUEST\n":rest) -> parseAddAdditionalGuest rest
  _ -> Left "Invalid command"

-- | Parses <hotel>
-- <hotel> ::= "HOTEL: " <text> "\n" | <hotel> "CHAIN OF" <hotel> | <hotel> <floor>
-- parseHotel :: String -> Maybe Hotel
-- parseHotel str =

-- <hotel> ::= "HOTEL: " <text> "\n" |  <hotel> "CHAIN OF " <hotel> | <hotel> <floor>
parseHotel :: [String] -> Either String Hotel
parseHotel input =
  case input of
    ("HOTEL: ":hotelName:rest) ->
      let (chain, remaining) = parseHotelChain rest
          (floors, finalRest) = parseFloors remaining
      in Right (Hotel hotelName chain floors, finalRest)
    _ -> Left "Invalid hotel format. Usage: <hotel> ::= \"HOTEL: \" <text> \"\n\" |  <hotel> \"CHAIN OF \" <hotel> | <hotel> <floor>"

-- parsing the hotel chain if such is present
parseHotelChain :: [String] -> (String, [String])
parseHotelChain [] = ("", [])
parseHotelChain ("CHAIN OF: ":chainName:rest) = (chainName, rest)
parseHotelChain rest = ("", rest)

-- parses floors
parseFloors :: [String] -> ([Floor], [String])
parseFloors [] = ([], [])
parseFloors input =
  case parseFloor input of
    Left _ -> ([], input) -- no more floors
    Right (floor, remaining) ->
      let (moreFloors, finalRest) = parseFloors remaining
      in (floor:moreFloors, finalRest)

-- <floor> ::= "FLOOR: " <number> "\n" <room>
parseFloor :: [String] -> Either String (Floor, [String])
parseFloor input =
  case input of
    ("FLOOR: ":numberStr:rest) ->
      let floorNumber = read numberStr :: Int
          (rooms, finalRest) = parseRooms rest
      in Right (Floor floorNumber rooms, finalRest)
    _ -> Left "Invalid floor format."

parseRooms :: [String] -> ([Room], [String])
parseRooms [] = ([], [])
parseRooms input =
  case parseRoom input of
    Left _ -> ([], input) -- no more rooms, use what is left
    Right (room, remaining) ->
      let (moreRooms, finalRest) = parseRooms remaining
      in (room : moreRooms, finalRest)

-- <room> ::= "ROOM: " <number> "\n" | <room> "ROOM SECTION " <room> | <room> <amenities> "\n"
parseRoom :: [String] -> Either String (Room, [String])
parseRoom input = 
  case input of
    ("ROOM: ":numberStr:"":rest) ->
      let roomNumber = read numberStr :: Int
          (sections, remaining) = parseRoomSections rest
          amenitiesList = parseAmenities remaining
      in Right (Room roomNumber sections amenitiesList, [])
    _ -> Left "Invalid room format."

parseRoomSections :: [String] -> ([Room], [String])
parseRoomSections [] = ([], [])
parseRoomSections input =
  case input of
    ("ROOM SECTION ":rest) ->
      let (roomSection, remaining) = parseRoom rest
          (sections, finalRest) = parseRoomSections remaining
      in (roomSection:sections, finalRest)
    _ -> ([], input) -- no more sections

parseAmenities :: [String] -> [Amenity]
parseAmenities [] = []
parseAmenities input =
  case input of
    ("AMENITIES: ":rest) ->
      let amenitiesList = splitAmenities rest
      in amenitiesList
    _ -> []

splitAmenities :: [String] -> [Amenity]
splitAmenities [] = []
splitAmenities input =
  let amenitiesStr = concat input -- combining input
      amenitiesWords = splitOnComma amenitiesStr
  in parseAmenitiesList amenitiesWords


parseAmenitiesList :: [String] -> [Amenity]
parseAmenitiesList [] = []
parseAmenitiesList (amenity:rest) =
  let parsedAmenity = parseAmenity amenity
  in if parsedAmenity == TV 
    || parsedAmenity == WiFi
    || parsedAmenity == Minibar
    || parsedAmenity == Balcony
    || parsedAmenity == AC

    then parsedAmenity : parseAmenitiesList rest
    else parseAmenitiesList rest


splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str =
  let (word, rest) = break isComma str
  in word : splitOnComma (dropWhile isSpace (drop 1 rest))
  where isComma c = c == ','


-- | main data types
parseGuest :: [String] -> Either String Guest
parseGuest input = 
  case input of
    ("GUEST: ":name:surname:[]) -> Right $ Guest name surname
    _ -> Left "Invalid guest format. GUEST: <name> <surname>"


parseCheckIn :: [String] -> Either String CheckIn
parseCheckIn input =
  case input of
    ("CHECK IN: ":dateStr:timeStr:[]) ->
       let date = parseDate dateStr
           time = parseTime timeStr
       in Right $ CheckIn date time
    _ -> Left "Invalid check-in format. CHECK IN: <date> <time>"


parseCheckOut :: [String] -> Either String CheckOut
parseCheckOut input =
  case input of
    ("CHECK OUT: ":dateStr:timeStr:[]) ->
      let date = parseDate dateStr
          time = parseTime timeStr
      in Right $ CheckOut date time
    _ -> Left "Invalid check-out format. CHECK OUT: <date> <time>"

parseDate :: String -> Date
parseDate dateStr =
  let [yearStr, monthStr, dayStr] = splitOnSpace dataStr
      year = read yearStr :: Int
      month = read monthStr :: Int
      day = read dayStr :: Int
  in Date year month day


parseTime :: String -> Time
parseTime timeStr =
  let [hourStr, minuteStr] = splitOnSpace timeStr
      hour = read hourStr :: Int
      minute = read minuteStr :: Int
  in Time hour minute

parsePrice :: [String] -> Either String Price
parsePrice input =
  case input of
    ("PRICE: ":priceStr:[]) -> Right $ Price (read priceStr :: Int)
    _ -> Left "Invalid price format. PRICE: <number>"

-- <make_reservation> ::= "MAKE RESERVATION\n" <guest> <hotel> <check_in> <check_out> <price>
parseMakeReservation :: [String] -> Either String Query
parseMakeReservation input =
  case parseGuest input of
    Left err -> Left err
    Right (guest, rest1) ->
      case parseHotel rest1 of
        Left err -> Left err
        Right (hotel, rest2) ->
          case parseCheckIn rest2 of
            Left err -> Left err
            Right (checkIn, rest3) ->
              case parseCheckOut rest3 of
                Left err -> Left err
                Right (checkOut, rest4) ->
                  case parsePrice rest4 of
                    Left err -> Left err
                    Right (price, _) -> Right (MakeReservation guest hotel checkIn checkOut price)


-- <add_hotel_room> ::= "ADD\n" <hotel> 
parseAdd :: [String] -> Either String Query
parseAdd input =
  case parseHotel input of
    Left err -> Left err
    Right hotel -> Right $ Add hotel


parseCancelReservation :: [String] -> Either String Query
parseCancelReservation input =
  case parseHotel input of
    Left err -> err
    Right (hotel, rest) ->
      case parseFloor rest of
        Left err -> err
        Right (floor, rest2) ->
          case parseRoom rest2 of
            Left err -> err
            Right (room, _) ->
              Right (CancelReservation hotel floor room)

parseRemoveHotelRoom :: [String] -> Either String Query
parseRemoveHotelRoom input =
  case parseHotel input of
    Left err -> err
    Right (hotel, rest) ->
      case parseFloor rest of
        Left err -> err
        Right (floor, rest2) ->
          case parseRoom rest2 of
            Left err -> err
            Right (room, _) ->
              Right (RemoveHotelRoom hotel floor room)

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
