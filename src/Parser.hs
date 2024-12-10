{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Parser (Parser(..), parseString) where

import Control.Applicative (Alternative(..))

data Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

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

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case runParser p1 inp of
            Right r1 -> Right r1
            Left e1 -> case runParser p2 inp of
                            Right r2 -> Right r2
                            Left e2 -> Left $ "Failed twise: " ++ e1 ++ " AND " ++ e2


parseString :: String -> Parser String
parseString s = Parser $ \input -> do
    if take (length s) input == s
        then
            return (s, drop (length s) input)
        else
            Left $ "Expected '" ++ s ++ "' but got: " ++ input