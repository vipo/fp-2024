{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson09 () where

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen

import Lessons.Lesson08(Parser(..))

import Control.Applicative

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

data Ohoho = Foo String | Bar Int deriving Show

instance Arbitrary Ohoho where
    arbitrary :: Gen Ohoho
    arbitrary =
        oneof [fmap Foo arbitrary, fmap Bar arbitrary]


instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case (runParser p1 inp) of
            Right r1 -> Right r1
            Left e1 -> case (runParser p2 inp) of
                            Right r2 -> Right r2
                            Left e2 -> Left $ "Failed twise: " ++ e1 ++ " AND " ++ e2

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
    case input of
        [] ->  Left ("Cannot find " ++ [c] ++ " in an empty input")
        s@(h:t) -> if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- >>> runParser parseA "fsdf"
-- Left "Failed twise: a is not found in fsdf AND A is not found in fsdf"
-- >>> runParser parseA "Add"
-- Right ('A',"dd")
parseA :: Parser Char
parseA = (parseChar 'a') <|> (parseChar 'A')

stateful :: State String Int
stateful = do
    value <- get
    let l = length value
    put "new state"
    return l

-- >>> runState combined "initial"
-- ((7,9),"new state")
combined :: State String (Int, Int)
combined = do
    a <- stateful
    b <- stateful
    return (a, b)

data State' s a = State' {
    runState' :: s -> (a, s)
}


get' :: State' a a
get' = State' $ \state -> (state, state)

put' :: s -> State' s ()
put' state = State' $ \_ -> ((), state)

instance Functor (State' s) where
    fmap :: (a -> b) -> State' s a -> State' s b
    fmap f functor = State' $ \inp ->
        case runState' functor inp of
            (a, ns) -> (f a, ns)

-- >>> runState' (fmap (\_ -> 5) (put' "some state")) "init"
-- (5,"some state")

-- >>> runState' (put' "some state") "init"
-- ((),"some state")

-- >>> runState' (get') "init"
-- ("init","init")

