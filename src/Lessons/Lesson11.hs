{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lessons.Lesson11 () where

import Control.Monad.Free (Free (..), liftF)

-- Initial
data Expr = Lit Integer
          | Add Expr Expr
          | Neg Expr
          deriving Show

-- lit :: Integer -> Expr
-- lit i = Lit i 

-- prog1 :: Expr
-- prog1 = lit 5

prog :: Expr
prog = Neg (Add (Lit 5) (Lit 6))

-- >>> eval prog
-- -11
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Neg e) = - (eval e)

-- >>> print' prog
-- "-(5 + 6)"
print' :: Expr -> String
print' (Lit i) = show i
print' (Add e1 e2) = (print' e1) ++ " + " ++ (print' e2)
print' (Neg e) = "-(" ++ print' e ++ ")"

-- Tagless Final
class Expression repr where
  lit :: Int -> repr
  add :: repr -> repr -> repr

instance Expression Int where
  lit :: Int -> Int
  lit a = a
  add :: Int -> Int -> Int
  add a b = a + b

instance Expression String where
  lit :: Int -> String
  lit = show
  add :: String -> String -> String
  add a b = a ++ " + " ++ b

-- >>> prog2
-- 11
prog2 :: Int
prog2 = add (lit 5) (lit 6)

-- >>> prog3
-- "5 + 6"
prog3 :: String
prog3 = add (lit 5) (lit 6)

class ExpressionNeg repr where
  neg :: repr -> repr

instance ExpressionNeg String where
  neg :: String -> String
  neg e = "-(" ++ e ++ ")"

-- >>> prog4
-- "-(5 + 6)"
prog4 :: String
prog4 = neg (add (lit 5) (lit 6))

-- prog5 :: Int
-- prog5 = neg (add (lit 5) (lit 6))

data GExpr a where
  GInt :: Integer -> GExpr Integer
  GBool :: Bool -> GExpr Bool
  GAdd :: GExpr Integer -> GExpr Integer -> GExpr Integer
  GAnd :: GExpr Bool -> GExpr Bool -> GExpr Bool
  GEq :: GExpr Integer -> GExpr Integer -> GExpr Bool

-- >>> evalG prog6
-- False
prog6 :: GExpr Bool
prog6 = GEq (GAdd (GInt 1) (GInt 3)) (GInt 5)

evalG :: GExpr a -> a
evalG (GInt i) = i
evalG (GBool b) = b
evalG (GAdd e1 e2) = evalG e1 + evalG e2
evalG (GAnd e1 e2) = evalG e1 && evalG e2
evalG (GEq e1 e2) = evalG e1 == evalG e2

data Ops = Eval Expr | Apply (Int -> Expr)

program :: [Ops]
program = [
    Eval prog, 
    Eval prog,
    Apply (\i -> prog)
  ]
