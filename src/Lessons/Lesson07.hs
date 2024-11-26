module Lessons.Lesson07 () where

-- >>> cp
-- [(1,'a'),(1,'a'),(1,'b'),(1,'b'),(1,'c'),(1,'c'),(1,'d'),(1,'d'),(1,'e'),(1,'e'),(1,'f'),(1,'f'),(1,'g'),(1,'g'),(1,'h'),(1,'h'),(1,'i'),(1,'i'),(1,'j'),(1,'j'),(1,'k'),(1,'k'),(2,'a'),(2,'a'),(2,'b'),(2,'b'),(2,'c'),(2,'c'),(2,'d'),(2,'d'),(2,'e'),(2,'e'),(2,'f'),(2,'f'),(2,'g'),(2,'g'),(2,'h'),(2,'h'),(2,'i'),(2,'i'),(2,'j'),(2,'j'),(2,'k'),(2,'k'),(3,'a'),(3,'a'),(3,'b'),(3,'b'),(3,'c'),(3,'c'),(3,'d'),(3,'d'),(3,'e'),(3,'e'),(3,'f'),(3,'f'),(3,'g'),(3,'g'),(3,'h'),(3,'h'),(3,'i'),(3,'i'),(3,'j'),(3,'j'),(3,'k'),(3,'k'),(4,'a'),(4,'a'),(4,'b'),(4,'b'),(4,'c'),(4,'c'),(4,'d'),(4,'d'),(4,'e'),(4,'e'),(4,'f'),(4,'f'),(4,'g'),(4,'g'),(4,'h'),(4,'h'),(4,'i'),(4,'i'),(4,'j'),(4,'j'),(4,'k'),(4,'k'),(5,'a'),(5,'a'),(5,'b'),(5,'b'),(5,'c'),(5,'c'),(5,'d'),(5,'d'),(5,'e'),(5,'e'),(5,'f'),(5,'f'),(5,'g'),(5,'g'),(5,'h'),(5,'h'),(5,'i'),(5,'i'),(5,'j'),(5,'j'),(5,'k'),(5,'k')]
cp :: [(Integer, Char)]
cp = [(a, b) | a <- [1..5], b <- ['a'..'k'], c <- [True, False]]

-- >>> cpm
-- [(1,'a'),(1,'a'),(1,'b'),(1,'b'),(1,'c'),(1,'c'),(1,'d'),(1,'d'),(1,'e'),(1,'e'),(1,'f'),(1,'f'),(1,'g'),(1,'g'),(1,'h'),(1,'h'),(1,'i'),(1,'i'),(1,'j'),(1,'j'),(1,'k'),(1,'k'),(2,'a'),(2,'a'),(2,'b'),(2,'b'),(2,'c'),(2,'c'),(2,'d'),(2,'d'),(2,'e'),(2,'e'),(2,'f'),(2,'f'),(2,'g'),(2,'g'),(2,'h'),(2,'h'),(2,'i'),(2,'i'),(2,'j'),(2,'j'),(2,'k'),(2,'k'),(3,'a'),(3,'a'),(3,'b'),(3,'b'),(3,'c'),(3,'c'),(3,'d'),(3,'d'),(3,'e'),(3,'e'),(3,'f'),(3,'f'),(3,'g'),(3,'g'),(3,'h'),(3,'h'),(3,'i'),(3,'i'),(3,'j'),(3,'j'),(3,'k'),(3,'k'),(4,'a'),(4,'a'),(4,'b'),(4,'b'),(4,'c'),(4,'c'),(4,'d'),(4,'d'),(4,'e'),(4,'e'),(4,'f'),(4,'f'),(4,'g'),(4,'g'),(4,'h'),(4,'h'),(4,'i'),(4,'i'),(4,'j'),(4,'j'),(4,'k'),(4,'k'),(5,'a'),(5,'a'),(5,'b'),(5,'b'),(5,'c'),(5,'c'),(5,'d'),(5,'d'),(5,'e'),(5,'e'),(5,'f'),(5,'f'),(5,'g'),(5,'g'),(5,'h'),(5,'h'),(5,'i'),(5,'i'),(5,'j'),(5,'j'),(5,'k'),(5,'k')]
cpm :: [(Integer, Char)]
cpm = do
    a <- [1..5]
    b <- ['a'..'k']
    c <- [True, False]
    return (a, b)

-- >>> cpm'
-- []
cpm' :: [(Char, Bool)]
cpm' = do
    a <- []
    b <- ['a'..'k']
    c <- [True, False]
    return (b, c)

-- >>> cpm''
-- []
cpm'' :: [(Integer, Char)]
cpm'' = do
    a <- [1..5]
    b <- ['a'..'k']
    c <- []
    return (a, b)

-- >>> mm
-- Just 42
mm :: Maybe Integer
mm = do
    a <- Just 42
    b <- Just 'a'
    return a

-- >>> mm'
-- Nothing
mm' :: Maybe Integer
mm' = do
    a <- Just 42
    b <- Nothing
    return a

-- >>> mm''
-- Just 43
mm'' :: Maybe Integer
mm'' = do
    a <- Just 42
    b <- Just "labas"
    return $ a + 1

-- >>> mm'''
-- Just 43
mm''' :: Maybe Integer
mm''' = do
    a <- Just 42
    return $ a + 1

-- >>> ml == map (\a -> a + 1) [1,2,3]
-- True
ml :: [Integer]
ml = do
    a <- [1,2,3]
    return $ a + 1

-- >>> em
-- Right (42,21)
em :: Either String (Int, Int)
em = do
    a <- Right 42
    b <- Right 21
    return (a, b)

-- >>> em'
-- Left "omg"
em' :: Either String Int
em' = do
    a <- Right 42
    b <- Left "omg"
    return a

-- >>> em''
-- Left "no"
em'' :: Either String Int
em'' = do
    _ <- Left "no"
    a <- Right 42
    b <- Left "omg"
    return a

-- >>> em'''
-- Left "no"
em''' :: Either String Int
em''' = do
    _ <- Left "no"
    a <- Right 42
    b <- Left "omg"
    Right a

-- >>> c == (Just 1 >>= (\z -> return (z + 1)) >>= (\x -> return (x, 'a')))
-- True
c :: Maybe (Int, Char)
c = do
    z <- Just 1
    x <- return $ z + 1
    return (x, 'a')

-- >>> fl
-- [2,4,6]
fl :: [Integer]
fl = fmap (\a -> a + a) [1,2,3]

-- >>> fm
-- Just 10
fm :: Maybe Integer
fm = fmap (\a -> a + a) $ Just 5

-- >>> fm'
-- Nothing
fm' :: Maybe Integer
fm' = fmap (\a -> a + a) $ Nothing

-- >>> fe
-- Right 6
fe :: Either String Integer
fe = fmap (\a -> a + 1) $ Right 5

-- >>> fe'
-- Left "omg"
fe' :: Either String Integer
fe' = fmap (\a -> a + 1) $ Left "omg"

-- >>> fet
-- Left 42
fet :: Either Integer Integer
fet = fmap (\a -> a + 1) $ Left 42

-- >>> addMaybies (Just 1) (Just 3)
-- Just 4
addMaybies :: Maybe Int -> Maybe Int -> Maybe Int
addMaybies a b = (+) <$> a <*> b

-- >>> addMaybies' (Just 1) (Just 3) (Just 1)
-- Just 5
addMaybies' :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addMaybies' a b c = (\x y z -> x + y + z) <$> a <*> b <*> c
