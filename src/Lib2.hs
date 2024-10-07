{-# LANGUAGE InstanceSigs #-}
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
  QueryAddHotelRoom AddHotelRoom |
  QueryRemoveHotelRoom RemoveHotelRoom |
  QueryMakeReservation MakeReservation |
  QueryCancelReservation CancelReservation |
  QueryAddAdditionalGuest AddAdditionalGuest

data Guest = Guest{
  guestName :: String,
  guestSurname :: String 
} deriving (Show, Eq)

data Hotel = Hotel {
  hotelName :: String,
  hotelChain :: Maybe String, -- possible that the hotel may not have a chain
  floors :: [Floor]
} deriving (Show, Eq)

data Floor = Floor {
  floorNumber :: Int,
  rooms :: [Room] -- floor can have many rooms attached to it
} deriving (Show, Eq)

data Room = Room {
  roomNumber :: Int,
  roomSections :: Maybe [Room],
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

data AddHotelRoom = AddHotelRoom {
  addHotel :: Hotel
} deriving (Show, Eq)

data RemoveHotelRoom = RemoveHotelRoom {
  removeHotel :: Hotel
} deriving (Show, Eq)


data MakeReservation = MakeReservation {
  reservationGuest :: Guest,
  reservationHotel :: Hotel,
  checkIn :: CheckIn,
  checkOut :: CheckOut,
  reservationPrice :: Price
} deriving (Show, Eq)

data CancelReservation = CancelReservation {
  cancelHotel :: Hotel
} deriving (Show, Eq)

data AddAdditionalGuest = AddAdditionalGuest {
  additionalGuest :: Guest,
  additionalHotel :: Hotel
} deriving (Show, Eq)



-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

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
