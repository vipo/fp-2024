{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
module Lessons.Lesson02 (Wheel(..)) where


-- >>> sum' []
-- 0
-- >>> sum' [1,2,3]
-- 6
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (h:t) = h + (sum' t)

-- >>> sum'' []
-- 0
-- >>> sum' [1]
-- 1
sum'' :: [Integer] -> Integer
sum'' l = sumTail l 0
    where
        sumTail [] acc = acc
        sumTail (h:t) acc = sumTail t (acc + h)

-- >>> dup []
-- []
-- >>> dup [1,2,3]
-- [1,1,2,2,3,3]
dup :: [a] -> [a]
dup [] = []
dup (h:t) = [h, h] ++ dup t

-- >>> dup'' []
-- []
-- >>> dup'' [1]
-- [1,1]
dup'' :: [a] -> [a]
dup'' l = 
    let
        empty = []
        dupTail [] acc = acc
        dupTail (h:t) acc = dupTail t ([h, h] ++ acc)
    in
        dupTail l empty

-- >>> safeDiv 1 0
-- Nothing
-- >>> safeDiv 10 2
-- Just 5
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

data MyBetterMaybe a = MyNothing | MyJust a

safeDiv' :: Integer -> Integer -> MyBetterMaybe Integer
safeDiv' _ 0 = MyNothing
safeDiv' a b = MyJust (a `div` b)

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

