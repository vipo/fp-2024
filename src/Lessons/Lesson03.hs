{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Lessons.Lesson03 () where

-- import Lessons.Lesson02(Wheel(..))
import Control.Exception (catch)

instance Show Wheel where
    show (Wheel i) = '⌀' : show i

-- >>> show (Wheel 5)
-- "\8960\&5"
data Wheel = Wheel Integer
data Pedals = One | Two
data Seat = Wooden | Plastic
data Unicycle = Unicycle Wheel Pedals Seat
--- >>> isSeatWooden (Unicycle (Wheel 100) Two Plastic)
-- False
-- >>> isSeatWooden (Unicycle (Wheel 200) Two Wooden)
-- True
isSeatWooden :: Unicycle -> Bool
isSeatWooden (Unicycle (Wheel x) _ Wooden) = True
isSeatWooden _ = False


data Person = Person String String Integer
data EmployeeId = EmployeId Integer deriving Show

employeeValidation :: Person -> Either String EmployeeId
employeeValidation (Person _ _ age) =
    if age < 14
        then Left "Too young"
        else
            if age > 67
                then Left "Too old"
                else Right (EmployeId 1)

--type Parser a = String -> a

-- bad: does not return what is left after parsing
charParser :: Char -> String -> Either String Char
charParser c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
charParser c s@(h:_) = if c == h then Right c else Left (c : " is not found in " ++ s)

-- >>> charParser' 'a' "abc"
-- Right ('a',"bc")
charParser' :: Char -> String -> Either String (Char, String)
charParser' c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
charParser' c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- >>> doubleCharParser 'a' "abc"
-- Left "a is not found in bc"
-- >>> doubleCharParser 'a' "aabc"
-- Right ('a',"bc")
-- >>> doubleCharParser 'a' "abac"
-- Left "a is not found in bac"
doubleCharParser :: Char -> String -> Either String (Char, String)
doubleCharParser c s =
     case charParser' c s of
        Left e1 -> Left e1
        Right (_, r1) ->
            case charParser' c r1 of
                Left e2 -> Left e2
                Right (c2, r2) -> Right (c2, r2)

