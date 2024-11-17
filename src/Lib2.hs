{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib2
  ( Query(..),
    Parser(..),
    parseString,
    query,
    BookInfo(..),
    ReaderInfo(..),
    BookGenre(..),
    BookAudience(..),
    State(..),
    emptyState,
    stateTransition,
    parseQuery,
    parseBorrowQuery,
    parseReturnQuery,
    parseAddBookQuery,
    parseAddReaderQuery,
    parseRemoveBookQuery,
    parseRemoveReaderQuery,
    parseMergeQuery,
    parseBookInfo,
    parseReaderInfo,
    parseBookGenre,
    parseBookAudience
  ) where
    
import Control.Applicative (Alternative (empty), (<|>), many, optional)  -- Added many, optional
import qualified Data.Char as C

data Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f functor = Parser $ \input ->
        case runParser functor input of
            Left e -> Left e
            Right (v, r) -> Right (f v, r)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Right (a, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    ff <*> fa = Parser $ \input ->
        case runParser ff input of
            Left e1 -> Left e1
            Right (f, r1) -> case runParser fa r1 of
                                Left e2 -> Left e2
                                Right (a, r2) -> Right (f a , r2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case (runParser p1 inp) of
            Right r1 -> Right r1
            Left e1 -> case (runParser p2 inp) of
                            Right r2 -> Right r2
                            Left e2 -> Left e2

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)

-- Basic Parsers

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> 
  case s of
    [] -> Left "No parser matched"
    (h:t) -> if c == h 
             then Right (c, t) 
             else Left "No parser matched"

parseLetter :: Parser Char
parseLetter = Parser $ \s -> case s of
  []       -> Left "No parser matched"
  (h : t)  -> if C.isLetter h
                 then Right (h, t)
                 else Left "No parser matched"

parseDigit :: Parser Char
parseDigit = Parser $ \s -> case s of
  []       -> Left "No parser matched"
  (h : t)  -> if C.isDigit h
                 then Right (h, t)
                 else Left "No parser matched"

parseString :: String -> Parser String
parseString [] = return []
parseString (c:cs) = do
    _ <- parseChar c
    rest <- parseString cs
    return (c : rest)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- Data Types
data Query
    = BorrowQuery BookInfo ReaderInfo
    | ReturnQuery BookInfo ReaderInfo
    | AddBookQuery BookInfo
    | AddReaderQuery ReaderInfo
    | RemoveBookQuery BookInfo
    | RemoveReaderQuery ReaderInfo
    | MergeQuery BookInfo (Maybe Query)
    deriving (Eq, Show)

data BookInfo = BookInfo Title Author BookGenre BookAudience
    deriving (Eq, Show)

type Title = String
type Author = String

data BookGenre = Fantasy | Detective | Scientific | Dictionary
    deriving (Show, Read, Eq)

data BookAudience = Children | Teenager | Adult
    deriving (Show, Read, Eq)

data ReaderInfo = ReaderInfo Name ReaderID
    deriving (Eq, Show)

type Name = String
type ReaderID = Int

query :: Parser Query
query =
    parseBorrowQuery
    <|> parseReturnQuery
    <|> parseAddBookQuery
    <|> parseAddReaderQuery
    <|> parseRemoveBookQuery
    <|> parseRemoveReaderQuery
    <|> parseMergeQuery


parseQuery :: String -> Either String Query
parseQuery s =
  case runParser query s of
    Left err -> Left err
    Right (q, r) -> if null r then Right q else Left ("Unrecognized characters:" ++ r)

-- Query Parsers
parseBorrowQuery :: Parser Query
parseBorrowQuery = do
    _ <- parseString "borrow "
    book <- parseBookInfo
    _ <- parseSpace
    reader <- parseReaderInfo
    return $ BorrowQuery book reader

parseReturnQuery :: Parser Query
parseReturnQuery = do
    _ <- parseString "return "
    book <- parseBookInfo
    _ <- parseSpace
    reader <- parseReaderInfo
    return $ ReturnQuery book reader

parseAddBookQuery :: Parser Query
parseAddBookQuery = do
    _ <- parseString "add-book "
    book <- parseBookInfo
    return $ AddBookQuery book

parseAddReaderQuery :: Parser Query
parseAddReaderQuery = do
    _ <- parseString "add-reader "
    reader <- parseReaderInfo
    return $ AddReaderQuery reader

parseRemoveBookQuery :: Parser Query
parseRemoveBookQuery = do
    _ <- parseString "remove-book "
    book <- parseBookInfo
    return $ RemoveBookQuery book

parseRemoveReaderQuery :: Parser Query
parseRemoveReaderQuery = do
    _ <- parseString "remove-reader "
    reader <- parseReaderInfo
    return $ RemoveReaderQuery reader

parseMergeQuery :: Parser Query
parseMergeQuery = do
    _ <- parseString "merge "
    book <- parseBookInfo
    rest <- optional $ do
        _ <- parseSpace
        parseMergeQuery
    return $ MergeQuery book rest

-- Book and Reader Info Parsers
parseBookInfo :: Parser BookInfo
parseBookInfo = do
  title <- parseTitle
  _ <- parseSpace
  author <- parseAuthor
  _ <- parseSpace
  genre <- parseBookGenre
  _ <- parseSpace
  audience <- parseBookAudience
  return $ BookInfo title author genre audience

parseTitle :: Parser Title
parseTitle = many1 parseLetter

parseAuthor :: Parser Author
parseAuthor = many1 parseLetter

parseBookGenre :: Parser BookGenre
parseBookGenre = parseData ["Fantasy", "Detective", "Scientific", "Dictionary"]

parseBookAudience :: Parser BookAudience
parseBookAudience = parseData ["Children", "Teenager", "Adult"]

parseData :: (Read a) => [String] -> Parser a
parseData = foldr (\str acc -> parseString str *> return (read str) <|> acc) empty

parseReaderInfo :: Parser ReaderInfo
parseReaderInfo = do
  name <- parseName
  _ <- parseSpace
  readerID <- parseReaderID
  return $ ReaderInfo name readerID

parseName :: Parser Name
parseName = many1 parseLetter

parseReaderID :: Parser ReaderID
parseReaderID = read <$> many1 parseDigit

-- States and State Transitions

data State = State { books :: [BookInfo], readers :: [ReaderInfo] }

emptyState :: State
emptyState = State [] []

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s (AddBookQuery book) =
    if bookExists book s
        then Left "Book already exists"
        else Right (Just (show book ++ " added."), addBook book s)

stateTransition s (AddReaderQuery reader) =
    if readerExists reader s
        then Left "Reader already exists"
        else Right (Just (show reader ++ " added."), addReader reader s)

stateTransition s (RemoveBookQuery book) =
    if not (bookExists book s)
        then Left "Book not found."
        else Right (Just (show book ++ " removed."), removeBook book s)

stateTransition s (RemoveReaderQuery reader) =
    if not (readerExists reader s)
        then Left "Reader not found."
        else Right (Just (show reader ++ " removed."), removeReader reader s)

stateTransition s (BorrowQuery book reader) =
    case borrowBook book reader s of
        Left err -> Left err
        Right newState -> Right (Just (show reader ++ " borrowed " ++ show book ++ "."), newState)

stateTransition s (ReturnQuery book reader) =
    case returnBook book reader s of
        Left err -> Left err
        Right newState -> Right (Just (show reader ++ " returned " ++ show book ++ "."), newState)

stateTransition s (MergeQuery book maybeNextQuery) =
    mergeBooks book maybeNextQuery s


bookExists :: BookInfo -> State -> Bool
bookExists book s = book `elem` books s

readerExists :: ReaderInfo -> State -> Bool
readerExists reader s = reader `elem` readers s

addBook :: BookInfo -> State -> State
addBook book s = s { books = book : books s }

addReader :: ReaderInfo -> State -> State
addReader reader s = s { readers = reader : readers s }

removeBook :: BookInfo -> State -> State
removeBook book s = s { books = filter (/= book) (books s) }

removeReader :: ReaderInfo -> State -> State
removeReader reader s = s { readers = filter (/= reader) (readers s) }

borrowBook :: BookInfo -> ReaderInfo -> State -> Either String State
borrowBook book reader s
    | not (bookExists book s) = Left "Book not found"
    | not (readerExists reader s) = Left "Reader not found"
    | otherwise = Right s 

returnBook :: BookInfo -> ReaderInfo -> State -> Either String State
returnBook book reader s
    | not (bookExists book s) = Left "Book not found"
    | not (readerExists reader s) = Left "Reader not found"
    | otherwise = Right s 

mergeBooks :: BookInfo -> Maybe Query -> State -> Either String (Maybe String, State)
mergeBooks book Nothing s = Right (Just "Book merged", addBook book s)
mergeBooks book (Just mergequery) s =
    case stateTransition (addBook book s) mergequery of
        Left err -> Left err
        Right (_, newState) -> Right (Just "Book merged and additional query processed", newState)

