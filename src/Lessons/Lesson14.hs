{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson14 () where

import Control.Monad.Trans.Class(lift, MonadTrans)

newtype EitherT e m a = EitherT {
    runEitherT :: m (Either e a)
}

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift ma = EitherT $ fmap Right ma

instance Monad m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f ta = EitherT $ do
    a <- runEitherT ta
    case a of
      Left e -> return $ Left e
      Right r -> return $ Right (f r)

instance Monad m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ return $ Right a
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  af <*> aa = EitherT $ do
    f <- runEitherT af
    case f of
      Left e1 -> return $ Left e1
      Right r1 -> do
        a <- runEitherT aa
        case a of
          Left e2 -> return $ Left e2
          Right r2 -> return $ Right (r1 r2)

instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  m >>= k = EitherT $ do
    a <- runEitherT m
    case a of
      Left e1 -> return $ Left e1
      Right r1 -> runEitherT (k r1)

type Parser a = EitherT String (State String) a

throwE :: Monad m => e -> EitherT e m a
throwE e = EitherT $ return $ Left e

parseChar :: Char -> Parser Char
parseChar a = do
    input <- lift get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ put xs >> return x
            else
                throwE $ a:" is not found"

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runEitherT parser)


-- >>> parse (parseTwoSameChars 'a') ""
-- (Left "Empty input","")
-- >>> parse (parseTwoSameChars 'a') "a"
-- (Left "Empty input","")
-- >>> parse (parseTwoSameChars 'a') "aaa"
-- (Right ('a','a'),"a")
parseTwoSameChars :: Char -> Parser (Char, Char)
parseTwoSameChars c = do
  c1 <- parseChar c
  c2 <- parseChar c
  return (c1, c2)
