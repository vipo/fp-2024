{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Lib2
    (Query(..),    
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Deck(..),
    Card(..),
    Rank(..),
    Suit(..),
    Number(..)
    )
where

import qualified Data.Char as C
import qualified Data.List as L

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  deriving (Show, Eq)

data Rank = RankNumber Number | Jack | Queen | King | Ace
  deriving (Eq)

instance Show Rank where
  show (RankNumber number) = show number
  show Jack                = "Jack"
  show Queen              = "Queen"
  show King               = "King"
  show Ace                = "Ace"

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq)

data Card = Card Rank Suit | Joker
  deriving (Eq)

instance Show Card where
  show (Card rank suit) = show rank ++ " of " ++ show suit
  show Joker            = "Joker"

data Deck = SingleCard Card | Deck Card Deck
  deriving (Eq)

instance Show Deck where
  show (Deck card deck) = show card ++ ", " ++ show deck
  show (SingleCard card) = show card

type Parser a = String -> Either String (a, String)

parseWord :: String -> [(String, a)] -> Parser a
parseWord typeName wordList input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then case L.lookup letters wordList of
            Just value -> Right (value, rest)
            Nothing    -> Left $ letters++" is not a "++typeName
        else Left $ input ++ " does not start with a letter"

-- <suit> ::= "Hearts" | "Diamonds" | "Clubs" | "Spades"
parseSuit :: Parser Suit
parseSuit = parseWord "Suit" [
    ("Hearts", Hearts),
    ("Diamonds", Diamonds),
    ("Clubs", Clubs),
    ("Spades", Spades)
  ]

-- <number> ::= "Two" | "Three" | "Four" | "Five" | "Six" | "Seven" | "Eight" | "Nine" | "Ten"
parseNumber :: Parser Number
parseNumber = parseWord "Number" [
    ("Two", Two),
    ("Three", Three),
    ("Four", Four),
    ("Five", Five),
    ("Six", Six),
    ("Seven", Seven),
    ("Eight", Eight),
    ("Nine", Nine),
    ("Ten", Ten)
  ]

or2 :: (a -> b) -> Parser a -> Parser b -> Parser b
or2 wrap p1 p2 input =
    case p1 input of
        Right (v1, r1) -> Right (wrap v1, r1)
        Left err1      -> 
            case p2 input of
                Right (v2, r2) -> Right (v2, r2)
                Left err2      -> Left (err1 ++ ", " ++ err2)


-- <rank> ::= <number> | "Jack" | "Queen" | "King" | "Ace"
parseRank :: Parser Rank
parseRank = or2 RankNumber parseNumber parseFaceCard
  where
    parseFaceCard :: Parser Rank
    parseFaceCard = parseWord "Face"[
        ("Jack", Jack),
        ("Queen", Queen),
        ("King", King),
        ("Ace", Ace)
      ]

and2 :: (a -> b -> c) -> Parser a -> String -> Parser b -> Parser c
and2 c a str b = \input ->
    case a input of
        Right (v1, r1) ->
            if take (length str) r1 == str then
                case b (drop (length str) r1) of
                    Right (v2, r2) -> Right (c v1 v2, r2)
                    Left e2 -> Left e2
            else
                Left $"'"++str++"' not found"
        Left e1 -> Left e1

-- <card> ::= <rank> "of" <suit> | "Joker"
parseCard :: Parser Card
parseCard = or2 id (and2 Card parseRank " of " parseSuit) parseJoker
  where
    parseJoker :: Parser Card
    parseJoker = parseWord "Joker" [("Joker", Joker)]

-- <deck> ::= <card> | <card> "," <deck>
parseDeck :: Parser Deck
parseDeck input = parseDeck' input
  where
    parseDeck' :: Parser Deck
    parseDeck' input =
        case parseCard input of
            Right (card, r1) ->
                case L.stripPrefix ", " r1 of
                    Just r2 ->
                        case parseDeck' r2 of
                            Right (deck, r3) -> Right (Deck card deck, r3)
                            Left _ -> Right (SingleCard card, r1)
                    Nothing -> Right (SingleCard card, r1)
            Left e -> Left e


data Query = ViewDeck | AddDeck Deck | DeleteDeck
  deriving(Show,Eq)

parseString :: String -> Parser String
parseString value input =
    if take (length value) input == value  
    then Right (value, drop (length value) input)
    else Left $ value ++ " is not found in "++input

-- <viewDeck> ::= "view"
parseView :: Parser Query
parseView input = 
  case parseString "view" input of
        Right ("view", rest) | all C.isSpace rest -> Right(ViewDeck, "")
        _ -> Left "Expected 'view'"  

-- <deleteDeck> ::= "delete"
parseDeleteDeck :: Parser Query
parseDeleteDeck input = case parseString "delete" input of
    Right ("delete", rest) | all C.isSpace rest -> Right (DeleteDeck, "") 
    _ -> Left "Expected 'delete'"

-- <addDeck> ::= "add" <deck>
parseAddDeck :: Parser Query
parseAddDeck input = 
    case parseString "add " input of
        Right (_, rest) ->
            case parseDeck rest of
                Right (deck, r) -> Right (AddDeck deck,r) 
                Left err -> Left $ "Failed to parse deck: " ++ err
        Left _ -> Left "Invalid command"

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' a b c= \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> 
                  case c input of
                    Right r3 -> Right r3
                    Left e3 -> Left  e3
                
-- <command> ::= <viewDeck> | <addDeck> | <deleteDeck>
parseQuery :: String -> Either String Query
parseQuery input = 
    case or3' parseView parseDeleteDeck parseAddDeck input of
        Right (query, _) -> Right query
        Left e -> Left e

data State = State (Maybe Deck)
  deriving(Eq,Show)

emptyState :: State
emptyState = State Nothing

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State maybeDeck) query = case query of
    ViewDeck -> 
        case maybeDeck of
            Just deck -> Right (Just (show deck), State maybeDeck)  
            Nothing -> Right (Just "The deck is empty.", State maybeDeck) 
    AddDeck newDeck -> 
        let updatedDeck = case maybeDeck of
                Just existingDeck -> mergeDecks newDeck existingDeck  
                Nothing -> newDeck  
            message = case newDeck of
                SingleCard _ -> "Card added." 
                Deck _ _ -> "Deck added."  
        in Right (Just message, State (Just updatedDeck)) 
    DeleteDeck ->
        Right (Just "Deck deleted.", State Nothing)  

mergeDecks :: Deck -> Deck -> Deck
mergeDecks (SingleCard card) existingDeck = Deck card existingDeck
mergeDecks (Deck card restOfDeck) existingDeck = Deck card (mergeDecks restOfDeck existingDeck)