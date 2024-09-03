module Lessons.Lesson01 (foo) where

foo :: Integer
foo = 42

bar :: Int
bar = 32

d :: Double
d = 4.0

b :: Bool
b = False

s :: String
s = "Labas"

c :: Char
c = 'a'

l :: [Integer]
l = [1,2,3]

ll :: [Integer]
ll = 0 : l

t :: (Integer, String)
t = (42, "medis")

tt :: (Int, Int, Int)
tt = (0, 0, 0)

fst' :: (a, b, c) -> a
fst' t =
    case t of
        (e1, _, _) -> e1

fst'' :: (a, b, c) -> a
fst'' (e1, _, _) = e1

head' :: [a] -> a
head' [] = error "omg"
head' (h:_) = h

add :: Integer -> Integer -> Integer
add a b = a + b

addOne :: Integer -> Integer
addOne = add 1

addOne' :: Integer -> Integer
addOne' a = add 1 a
