{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L
-- | An entity which represents user input.
data Query
    = CreateCommand Toy
    | ModifyCommand Toy Modification
    | QueryCommand QueryType
    deriving (Eq, Show)

data Toy
    = SimpleToy String
    | CompositeToy [Toy]
    | DecoratedToy Toy Decoration
    deriving (Eq, Show)

data Modification
    = AddDecoration Decoration
    | RemoveDecoration Decoration
    deriving (Eq, Show)

data QueryType
    = AllToys
    | ToysWithDecoration Decoration
    | ToysOfType ToyType
    deriving (Eq, Show)

data ToyType = Simple | Composite | Decorated deriving (Eq, Show)

data Decoration
    = Color String
    | Size String
    | Material String
    deriving (Eq, Show)

-- Custom alternative for Maybe
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input =
    case parseCommand (stripSpaces input) of
        Just (command, "") -> Right command
        Just (_, leftover) -> Left $ "Parsing failed, leftover input: " ++ leftover
        Nothing -> Left "Invalid command format"

parseCommand :: String -> Maybe (Query, String)
parseCommand input =
    parseCreateCommand input `orElse`
    parseModifyCommand input `orElse`
    parseQueryCommand input

parseCreateCommand :: String -> Maybe (Query, String)
parseCreateCommand input = do
    rest <- parseLiteral "create(" input
    (toy, rest') <- parseToy rest
    rest'' <- parseLiteral ")" rest'
    return (CreateCommand toy, rest'')

parseModifyCommand :: String -> Maybe (Query, String)
parseModifyCommand input = do
    rest <- parseLiteral "modify(" input
    (toy, rest') <- parseToy rest
    rest'' <- parseLiteral "," rest'
    (mod, rest''') <- parseModification rest''
    rest'''' <- parseLiteral ")" rest'''
    return (ModifyCommand toy mod, rest'''')

parseQueryCommand :: String -> Maybe (Query, String)
parseQueryCommand input = do
    rest <- parseLiteral "query(" input
    (queryType, rest') <- parseQueryType rest
    rest'' <- parseLiteral ")" rest'
    return (QueryCommand queryType, rest'')

parseModification :: String -> Maybe (Modification, String)
parseModification input =
    parseAddDecoration input `orElse`
    parseRemoveDecoration input

parseAddDecoration :: String -> Maybe (Modification, String)
parseAddDecoration input = do
    rest <- parseLiteral "add(" input
    (decoration, rest') <- parseDecoration rest
    rest'' <- parseLiteral ")" rest'
    return (AddDecoration decoration, rest'')

parseRemoveDecoration :: String -> Maybe (Modification, String)
parseRemoveDecoration input = do
    rest <- parseLiteral "remove(" input
    (decoration, rest') <- parseDecoration rest
    rest'' <- parseLiteral ")" rest'
    return (RemoveDecoration decoration, rest'')

parseQueryType :: String -> Maybe (QueryType, String)
parseQueryType input =
    (parseLiteral "all_toys" input >>= \rest -> return (AllToys, rest)) `orElse`
    parseToysWithDecoration input `orElse`
    parseToysOfType input

parseToysWithDecoration :: String -> Maybe (QueryType, String)
parseToysWithDecoration input = do
    rest <- parseLiteral "toys_with_decoration(" input
    (decoration, rest') <- parseDecoration rest
    rest'' <- parseLiteral ")" rest'
    return (ToysWithDecoration decoration, rest'')

parseToysOfType :: String -> Maybe (QueryType, String)
parseToysOfType input = do
    rest <- parseLiteral "toys_of_type(" input
    (toyType, rest') <- parseToyType rest
    rest'' <- parseLiteral ")" rest'
    return (ToysOfType toyType, rest'')

parseToyType :: String -> Maybe (ToyType, String)
parseToyType input =
    (parseLiteral "simple" input >>= \rest -> return (Simple, rest)) `orElse`
    (parseLiteral "composite" input >>= \rest -> return (Composite, rest)) `orElse`
    (parseLiteral "decorated" input >>= \rest -> return (Decorated, rest))

parseToy :: String -> Maybe (Toy, String)
parseToy input =
    parseSimpleToy input `orElse`
    parseCompositeToy input `orElse`
    parseDecoratedToy input

parseSimpleToy :: String -> Maybe (Toy, String)
parseSimpleToy input =
    (parseLiteral "ball" input >>= \rest -> return (SimpleToy "ball", rest)) `orElse`
    (parseLiteral "doll" input >>= \rest -> return (SimpleToy "doll", rest)) `orElse`
    (parseLiteral "car" input >>= \rest -> return (SimpleToy "car", rest)) `orElse`
    (parseLiteral "robot" input >>= \rest -> return (SimpleToy "robot", rest)) `orElse`
    (parseLiteral "train" input >>= \rest -> return (SimpleToy "train", rest))

parseCompositeToy :: String -> Maybe (Toy, String)
parseCompositeToy input = do
    rest <- parseLiteral "combine(" input
    (toys, rest') <- parseToyList rest
    rest'' <- parseLiteral ")" rest'
    return (CompositeToy toys, rest'')

parseToyList :: String -> Maybe ([Toy], String)
parseToyList input = do
    (toy, rest) <- parseToy input
    case parseLiteral "," rest of
        Just rest' -> do
            (toys, rest'') <- parseToyList rest'
            return (toy : toys, rest'')
        Nothing -> return ([toy], rest)

parseDecoratedToy :: String -> Maybe (Toy, String)
parseDecoratedToy input = do
    rest <- parseLiteral "decorate(" input
    (toy, rest') <- parseToy rest
    rest'' <- parseLiteral "," rest'
    (decoration, rest''') <- parseDecoration rest''
    rest'''' <- parseLiteral ")" rest'''
    return (DecoratedToy toy decoration, rest'''')

parseDecoration :: String -> Maybe (Decoration, String)
parseDecoration input =
    (parseLiteral "red" input >>= \rest -> return (Color "red", rest)) `orElse`
    (parseLiteral "blue" input >>= \rest -> return (Color "blue", rest)) `orElse`
    (parseLiteral "green" input >>= \rest -> return (Color "green", rest)) `orElse`
    (parseLiteral "yellow" input >>= \rest -> return (Color "yellow", rest)) `orElse`
    (parseLiteral "small" input >>= \rest -> return (Size "small", rest)) `orElse`
    (parseLiteral "medium" input >>= \rest -> return (Size "medium", rest)) `orElse`
    (parseLiteral "large" input >>= \rest -> return (Size "large", rest)) `orElse`
    (parseLiteral "plastic" input >>= \rest -> return (Material "plastic", rest)) `orElse`
    (parseLiteral "wood" input >>= \rest -> return (Material "wood", rest)) `orElse`
    (parseLiteral "metal" input >>= \rest -> return (Material "metal", rest))

stripSpaces :: String -> String
stripSpaces = dropWhile (== ' ')

parseLiteral :: String -> String -> Maybe String
parseLiteral literal input
    | literal `isPrefixOf` input = Just (drop (length literal) input)
    | otherwise = Nothing
  where
    isPrefixOf = (==) . take (length literal)

-- | Represents the program's state.
data State = State
    { toys :: [Toy]
    } deriving (Eq, Show)

-- | Creates an initial program's state.
emptyState :: State
emptyState = State { toys = [] }

-- | Updates a state according to a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
    CreateCommand toy -> Right (Just "Toy created", state { toys = toy : toys state })
    ModifyCommand _ _ -> Left "ModifyCommand not implemented"
    QueryCommand AllToys -> Right (Just (show $ toys state), state)
    _ -> Left "Unsupported query"
