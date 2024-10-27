{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
-- A module - a single unit of compilation


import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

-- We will parse the BNF:
-- <name> ::= <word> " " <word> | <number> | <string>
data MyWord = MyWord String deriving Show
data MyName = WordName MyWord MyWord
              | NumberName Int
              | StringName String
              deriving Show

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

        
or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- <word>
parseWord :: Parser MyWord
parseWord input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (MyWord letters, rest)
        else Left (input ++ " does not start with a letter")

-- <number>
parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)
        
-- <word> " " <word>
parseWordName :: Parser MyName
parseWordName input = (and3' (\a _ b -> WordName a b) parseWord (parseChar ' ') parseWord) input

parseNumberName :: Parser MyName
parseNumberName input = 
    case parseNumber input of
        Right (num, rest) -> Right (NumberName num, rest)
        Left err -> Left err

-- <string>
parseStringName :: Parser MyName
parseStringName input = Right (StringName input, "")

-- <name> ::= <word> " " <word> | <number> | <string>
parseName :: Parser MyName
parseName input = (parseWordName `or2` parseNumberName `or2` parseStringName) input
        
main :: IO ()
main = do
  print (parseName "hello there world!") -- output: Right (WordName (MyWord "hello") (MyWord "there")," world!")
  print (parseName "123 there world!") -- output: Right (NumberName 123," there world!")
  print (parseName ",,,, 123 there world!") -- output: Right (StringName ",,,, 123 there world!","")