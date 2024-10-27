{-# LANGUAGE InstanceSigs #-}
module Lib2 
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    ArtPiece(..),
    ArtType(..),
    Artist(..),
    ) where
import Data.Char

-- | An entity which represets user input.
data Query
  = AddArtwork ArtPiece
  | SellArtwork ArtPiece
  | PrintInfo 
  | Sequence [Query] -- Sequence of queries
  deriving (Eq, Show)

data ArtPiece = ArtPiece {
    artId        :: Int,
    title        :: String,
    artType      :: ArtType,
    price        :: Double,
    description  :: String
} deriving (Eq, Show)

data ArtType = Painting | Photograph | Sculpture | Digital | Drawing | Sketch
             deriving (Eq, Show)

data Artist = Artist String String
            deriving (Eq, Show)

type Parser a = String -> Either String (a, String)

-- simple parsers

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

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

parseInteger :: Parser String
parseInteger input =
  let input' = skipSpaces input
      (digits, rest) = span isDigit input'
  in if null digits
       then Left "Expected an integer but found no digits"
       else Right (digits, rest)

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and3' :: (a1 -> a2 -> a3 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser d
and3' f a1 a2 a3 = \input ->
    case a1 input of
        Right (v1, r1) ->
          case a2 r1 of
            Right(v2, r2) ->
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

and5' :: (a1 -> a2 -> a3 -> a4 -> a5 -> d) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser a4 -> Parser a5 -> Parser d
and5' f a1 a2 a3 a4 a5 = \input ->
  case a1 input of
    Right (v1, r1) ->
      case a2 r1 of
        Right (v2, r2) ->
          case a3 r2 of
            Right (v3, r3) ->
              case a4 r3 of
                Right (v4, r4) ->
                  case a5 r4 of
                    Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' p1 p2 p3 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left e1 -> case p2 input of
      Right r2 -> Right r2
      Left e2 -> case p3 input of
        Right r3 -> Right r3
        Left e3 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3)
-- BNF parsers

-- <id> ::= <integer>
parseNumber :: Parser Int
parseNumber input =
  let input' = skipSpaces input
      (digits, rest) = span isDigit input'
  in if null digits
       then Left "Expected a number but found no digits"
       else Right (read digits, rest)

-- <title> ::= <string>
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



-- <price> ::= <intiger> "." <intiger>

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

-- <art_type> ::= "Painting" | "Photograph" | "Sculpture" | "Digital" | "Drawing" | "Sketch"

parseArtType :: Parser ArtType
parseArtType input =
  let input' = skipSpaces input
      options = [("Painting", Painting), ("Photograph", Photograph), ("Sculpture", Sculpture),
                 ("Digital", Digital), ("Drawing", Drawing), ("Sketch", Sketch)]
      tryParse [] = Left "Expected an art type but found none"
      tryParse ((optStr, optType):opts) =
        case parseLiteral optStr input' of
          Right (_, rest) -> Right (optType, rest)
          Left _ -> tryParse opts
  in tryParse options

-- <art_piece> ::= <id> <title> <type> <price> <description> 

parseArtPiece :: Parser ArtPiece
parseArtPiece =
  and5'
    ArtPiece
    parseNumber   
    parseString          
    parseArtType        
    parsePrice         
    parseString         

-- <print_info> ::= "print_info"

-- <print_info> ::= "print_info" "(" ")"
parsePrintInfo :: Parser Query
parsePrintInfo =
  and3'
    (\_ _ _ -> PrintInfo)
    (parseLiteral "print_info")
    (parseChar '(')
    (parseChar ')')

-- <sell_artwork> ::= "sell_artwork "(<art_piece>)" "

parseSellArtwork :: Parser Query
parseSellArtwork =
  and4'
    (\_ _ artWork _ -> SellArtwork artWork)
    (parseLiteral "sell_artwork")
    (parseChar '(')
    parseArtPiece
    (parseChar ')')

-- <add_artwork> ::= "add_artwork" <art_piece>

parseAddArtwork :: Parser Query
parseAddArtwork =
  and4'
    (\_ _ artWork _ -> AddArtwork artWork)
    (parseLiteral "sell_artwork")
    (parseChar '(')
    parseArtPiece
    (parseChar ')')

-- <command> ::= <add_artwork> | <sell_artwork> | <print_info>

parseCommand :: Parser Query
parseCommand = or3' parseAddArtwork parseSellArtwork parsePrintInfo

parseCommands :: Parser [Query]
parseCommands input = case parseCommand input of
  Right (firstQuery, rest) ->
    case parseChar ';' rest of
      Right (_, afterSemicolon) -> case parseCommands afterSemicolon of
        Right (otherQueries, finalRest) -> Right (firstQuery : otherQueries, finalRest)
        Left err -> Left err
      Left _ -> Right ([firstQuery], rest)
  Left _ -> Left "Unrecognized command"

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input = case parseCommands input of
  Right (queries, _) -> Right (Sequence queries)
  Left err -> Left err

-- | An entity which represents your program's state.
data State = State
  { artworks :: [ArtPiece] 
  }
  deriving (Eq, Show)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {artworks = []}


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (AddArtwork art) =
  -- Check if the artwork already exists in the state by artId.
  if any (\a -> artId a == artId art) (artworks state)
    then Left "Artwork with the same ID already exists."
    else Right (Just "Artwork added.", state { artworks = art : artworks state })

stateTransition state (SellArtwork art) =
  -- Remove artwork by ID, ensuring it exists in the state.
  let updatedArtworks = filter (\a -> artId a /= artId art) (artworks state)
  in if length updatedArtworks == length (artworks state)
       then Left "Artwork not found for sale."
       else Right (Just "Artwork sold.", state { artworks = updatedArtworks })

stateTransition state PrintInfo =
  -- Display the current state of artworks.
  let info = if null (artworks state)
               then "No artworks available."
               else unlines (map show (artworks state))
  in Right (Just info, state)

stateTransition state (Sequence queries) =
  -- Process a sequence of queries, updating the state sequentially.
  foldl (\acc q -> case acc of
           Left err -> Left err
           Right (_, st) -> stateTransition st q)
        (Right (Nothing, state)) queries
