{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
module Lessons.Lesson05 () where

import Lessons.Lesson04(Parser, parseNumber, and2', parseChar)

-- listOfNumbers := number (,number)*
--                                  ^ many

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r

-- >>> parseManyAs ""
-- Right ("","")
-- >>> parseManyAs "aaab"
-- Right ("aaa","b")
-- >>> parseManyAs "baaab"
-- Right ("","baaab")
parseManyAs :: Parser [Char]
parseManyAs = many (parseChar 'a')

-- >>> parseListOfNumbers ""
-- Left "empty input, cannot parse a number"
-- >>> parseListOfNumbers "a"
-- Left "not a number"
-- >>> parseListOfNumbers "123"
-- Right ([123],"")
-- >>> parseListOfNumbers "123,123123,31232"
-- Right ([123,123123,31232],"")

parseListOfNumbers :: Parser [Integer]
parseListOfNumbers = and2' (:)
                        parseNumber
                        (many (and2' (\_ b -> b) (parseChar ',') parseNumber))

-- >>> parseTwoNumbers "3123"
-- Left "Cannot find , in an empty input"
-- >>> parseTwoNumbers "3123,"
-- Left "empty input, cannot parse a number"
-- >>> parseTwoNumbers "3123,4234a"
-- Right ((3123,4234),"a")
parseTwoNumbers :: Parser (Integer, Integer)
parseTwoNumbers = and2' (\a b -> (a, b)) parseNumber (and2' (\_ b -> b) (parseChar ',') parseNumber)


data Person = Person String Int

surname :: Person -> String
surname (Person s _) = s

-- >>> Person' "Vi" 13
-- Person' {name = "Vi", age = 13}
data Person' = Person' {
    name :: String,
    age :: Int
} deriving Show

-- >>> me {age = 14, name = "Po"}
-- Person' {name = "Po", age = 14}
-- >>> :t name
-- name :: Person' -> String
-- >>> name me
-- "Vi"
me :: Person'
me = Person' "Vi" 13

pureBusinessLogic :: String -> String
pureBusinessLogic n = "Hello, " ++ n

game :: IO String
game =
    do
        putStrLn "What is your name?"
        name' <- getLine
        return (pureBusinessLogic name')

