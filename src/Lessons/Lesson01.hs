{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
-- A module - a single unit of compilation
module Lessons.Lesson01 (foo) where

-- Integer is an "endless" integer value:
-- the value can be as big as needed
foo :: Integer
--     ^
--     this is a type
foo = 42
--    ^
--    this is a value

-- Int depends on your architecture
bar :: Int
bar = 32

d :: Double
d = 4.0

-- Bool has two values
b :: Bool
b = False

-- String is a list of Char's,
-- String and [Char] are synonims
s :: String
s = "Labas"

-- A single char
c :: Char
c = 'a'

-- A list of Integers
l :: [Integer]
l = [1,2,3]

-- Lists in Haskell are single linked list
-- All list are either empty ([]), or
-- contain (are constructed of) TWO vales:
-- head (single elemnt)
-- tail (a list of elements which folloe a head)
ll :: [Integer]
ll = 0 : l
--     ^
--     prepends an element to an already existing
--     list (which might be empty)

-- A tuple, in this case it contains two values - it's a pair
t :: (Integer, String)
t = (42, "medis")
-- We can access a pair's elements using functions
-- >>> fst t
-- 42
-- >>> snd t
-- "medis"

-- Three elements tuple
tt :: (Int, Int, Int)
tt = (0, 0, 0)

-- fst works with pairs only
-- >>> :t fst
-- fst :: (a, b) -> a

-- so we have to use pattern matching to
-- deconstruct a tuple and extract values
fst' :: (a, b, c) -> a
fst' t =
    case t of -- pattern match
        (e1, _, _) -> e1
--      ^
--      a pattern

-- we can pattern match directly in function arguments
fst'' :: (a, b, c) -> a
fst'' (e1, _, _) = e1
--         ^
--         we are not assigning any name

-- we can use pattern matching on lists
head' :: [a] -> a
head' [] = error "omg" --this throws an exception
head' (h:_) = h

-- Up untill now we had function with one argument
-- add has two arguments
-- >>> add 1 1
-- 2
-- >>> add 4 4
-- 8
add :: Integer -> Integer -> Integer
add a b = a + b

-- If we pass less arguments than needed,
-- we get a function which requires all
-- remaining arguments. This is caller currying
-- >>> :t add 1
-- add 1 :: Integer -> Integer
addOne :: Integer -> Integer
addOne = add 1
--     ^
--     we take no arguments,
--     since addOne and (add 1) have the same type
--     This called eta reduction

addOne' :: Integer -> Integer
addOne' a = add 1 a
--      ^
--      we can provide explicit argument
--      and not use eta reduction