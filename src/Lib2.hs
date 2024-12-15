
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
    Number(..)
    )
where
  
import Control.Applicative (Alternative (empty), optional, (<|>))
import Parsers


-- <command> ::= <viewDeck> | <addDeck> | <deleteDeck>
parseQuery :: String -> Either String Query
parseQuery input =
    let (result, _) = parse (parseView <|> parseDelete <|> parseAddDeck) input
    in result

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


