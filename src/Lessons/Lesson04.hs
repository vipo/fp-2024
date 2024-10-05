{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lessons.Lesson04 () where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

-- >>> parseChar 'a' "aaa"
-- Right ('a',"aa")
-- >>> parseChar '*' "fdf"
-- Left "* is not found in fdf"
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- >>> parseLetter "fsdf"
-- Right ('f',"sdf")
-- >>> parseLetter " sdf"
-- Left " sdf does not start with a letter"
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

-- >>> and2 parseLetter parseLetter "hi"
-- Right (('h','i'),"")
-- >>> and2 parseLetter parseLetter "43"
-- Left "43 does not start with a letter"
-- >>> and2 parseLetter parseLetter "a43"
-- Left "43 does not start with a letter"
and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right ((v1, v2), r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

data CharPair = CharPair Char Char
    deriving Show

-- >>> and2' CharPair parseLetter parseLetter "dfdf"
-- Right (CharPair 'd' 'f',"df")
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

-- >>> parseDigit "432"
-- Right ('4',"32")
-- >>> parseDigit "labas"
-- Left "labas does not start with a digit"
parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h:t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)
-- >>> parseAlphaNum "a2"
-- Right ('a',"2")
-- >>> parseAlphaNum "2a"
-- Right ('2',"a")
-- >>> parseAlphaNum " "
-- Left "  does not start with a letter,   does not start with a digit"
parseAlphaNum :: Parser Char
parseAlphaNum = or2 parseLetter parseDigit

-- >>> parseNumber "123d"
-- Right (123,"d")
parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)
