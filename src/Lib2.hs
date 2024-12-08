
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
    Number(..),
    parseCard,
    parseDeck,
    convertParser,
    parseQuery'
    )
where

import Control.Applicative (Alternative ((<|>)))

import qualified Data.Char as C
import qualified Data.List as L
import qualified Parser as P

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

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 = \input ->
    case p1 input of
        Left e -> Left e
        Right (v1, r1) ->
            case p2 r1 of
                Left e -> Left e
                Right (v2, r2) -> Right (f v1 v2, r2)

and2s :: (a -> b -> c) -> Parser a -> Parser String -> Parser b -> Parser c
and2s c a strParser b = \input ->
    case a input of
        Right (v1, r1) ->
            case strParser r1 of
                Right (_, r2) ->
                    case b r2 of
                        Right (v2, r3) -> Right (c v1 v2, r3)
                        Left e2 -> Left e2
                Left eStr -> Left eStr
        Left e1 -> Left e1

orX :: [Parser a] -> Parser a
orX parsers = orX' parsers []
  where
    orX' :: [Parser a] -> [String] -> Parser a
    orX' [] errors _  = Left (L.intercalate ", " errors)
    orX' (p:ps) errors input=
        case p input of
            Right result -> Right result
            Left errMsg -> orX' ps  (errors ++ [errMsg]) input

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r

parseString :: String -> Parser String
parseString value input =
    if take (length value) input == value
    then Right (value, drop (length value) input)
    else Left "Invalid input"

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

-- <rank> ::= <number> | "Jack" | "Queen" | "King" | "Ace"
parseRank :: Parser Rank
parseRank = orX [parseNumberAsRank, parseFaceCard]
  where
    parseFaceCard :: Parser Rank
    parseFaceCard = parseWord "Face" [
        ("Jack", Jack),
        ("Queen", Queen),
        ("King", King),
        ("Ace", Ace)
      ]
    parseNumberAsRank :: Parser Rank
    parseNumberAsRank input =
        case parseNumber input of
            Right (number, rest) -> Right (RankNumber number, rest)
            Left err -> Left err

-- <card> ::= <rank> "of" <suit> | "Joker"
parseCard :: Parser Card
parseCard = orX [and2s Card parseRank (parseString " of ") parseSuit, parseJoker]
  where
    parseJoker :: Parser Card
    parseJoker = parseWord "Joker" [("Joker", Joker)]


and3 :: (a -> b) -> (b->b->b)-> Parser a -> Parser String -> Parser b -> Parser b
and3 w mrg a strParser b = \input ->
    case a input of
        Right (v1, r1) ->
            case strParser r1 of
                Right (_, r2) ->
                    case b r2 of
                        Right (v2, r3) -> Right (mrg (w v1) v2, r3)
                        Left e2 -> Left e2
                Left eStr -> Left eStr
        Left e1 -> Left e1

-- <deck> ::= <card> "," <deck> | <card> 
parseDeck :: Parser Deck
parseDeck = orX[and3 SingleCard mergeDecks parseCard (parseString ", ") parseDeck,parseSingleDeck]
  where 
    parseSingleDeck :: Parser Deck
    parseSingleDeck input= case parseCard input of
      Left e -> Left e
      Right (v,r)->Right(SingleCard v,r)   

data Query = ViewDeck | AddDeck Deck | DeleteDeck
  deriving(Show,Eq)

-- <viewDeck> ::= "view"
parseView :: Parser Query
parseView input =
  case parseString "view" input of
      Right ("view", rest) ->
          if all C.isSpace rest
          then Right (ViewDeck, "")
          else Left "Expected only whitespace after 'view'"
      _ -> Left "Expected 'view'"

-- <deleteDeck> ::= "delete"
parseDeleteDeck :: Parser Query
parseDeleteDeck input =
  case parseString "delete" input of
    Right ("delete", rest) ->
        if all C.isSpace rest
        then Right (DeleteDeck, "")
        else Left "Expected only whitespace after 'delete'"
    _ -> Left "Expected 'delete'"

-- <addDeck> ::= "add" <deck>
parseAddDeck :: Parser Query
parseAddDeck = and2 (\_ deck -> AddDeck deck) (parseString "add ") parseAdd
  where
    parseAdd :: Parser Deck
    parseAdd input =
        case parseDeck input of
            Right (deck, r) -> Right (deck, r)
            Left err -> Left $ "Failed to parse deck: " ++ err

-- <command> ::= <viewDeck> | <addDeck> | <deleteDeck>
parseQuery :: String -> Either String Query
parseQuery input =
    case orX [parseView,parseDeleteDeck,parseAddDeck] input of
        Right (query, _) -> Right query
        Left e-> Left e

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


convertParser :: Parser a -> P.Parser a
convertParser p= P.Parser {P.runParser = p}

parseQuery' :: P.Parser Query
parseQuery' =
   parseView'
    <|> parseDeleteDeck'
    <|> parseAddDeck'

parseView' :: P.Parser Query
parseView' = do
  _ <- P.parseString "view"
  return ViewDeck

parseDeleteDeck' :: P.Parser Query
parseDeleteDeck' = do
  _ <- P.parseString "delete"
  return DeleteDeck

parseAddDeck' :: P.Parser Query
parseAddDeck' = do
  _ <- P.parseString "add "
  deck <- convertParser parseDeck
  return (AddDeck deck)




