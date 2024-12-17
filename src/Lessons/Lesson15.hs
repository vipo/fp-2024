{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson15 () where
import qualified Data.List as L

data Term = Var String | Abs String Term | App Term Term

instance Show Term where
  show :: Term -> String
  show (Var n) = n
  show (Abs n t) = concat ["(/|", n, ".", show t, ")"]
  show (App t1 t2) = concat ["(", show t1, " ", show t2, ")"]

-- >>> show tru
-- "(/|t.(/|f.t))"
tru :: Term
tru = Abs "t" (Abs "f" (Var "t"))

-- >>> show fls
-- "(/|t.(/|f.f))"
fls :: Term
fls = Abs "t" (Abs "f" (Var "f"))

-- >>> show c0
-- "(/|t.(/|z.z))"
c0 :: Term
c0 = Abs "s" (Abs "z" (Var "z"))

-- >>> show c2
-- "(/|t.(/|z.(s (s z))))"
c2 :: Term
c2 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))

apps :: [Term] -> Term
apps [] = error "Empty application"
apps [_] = error "Two term needed to application"
apps (t1 : t2 : ts) = apps' (App t1 t2) ts
  where
    apps' t [] = t
    apps' t (x : xs) = apps' (App t x) xs

-- >>> show land
-- "(/|b.(/|c.((b c) (/|t.(/|f.f)))))"
land :: Term
land = Abs "b" $ Abs "c" $ apps [Var "b", Var "c", fls]

-- >>> show expr
-- "(((/|b.(/|c.((b c) (/|t.(/|f.f))))) (/|t.(/|f.t))) (/|t.(/|f.f)))"
expr :: Term
expr = apps [land, tru, tru]

plus :: Term
plus = Abs "m" $ Abs "n" $ Abs "s" $ Abs "z" $ apps [Var "m", Var "s", apps [Var "n", Var "s", Var "z"]]

data ITerm
  = IVar Int
  | IAbs ITerm
  | IApp ITerm ITerm
  deriving (Eq)

instance Show ITerm where
  show :: ITerm -> String
  show (IVar i) = show i
  show (IAbs t) = concat ["(/|.", show t, ")"]
  show (IApp t1 t2) = concat ["(", show t1, " ", show t2, ")"]

-- >>> deBruijnIndecs [] tru
-- Variable not in scope:
--   deBruijnIndecs :: [a0_aZLd[tau:1]] -> Term -> t_aZLa[sk:1]
deBruijnIndices :: [String] -> Term -> ITerm
deBruijnIndices ctx t = walk [] t
  where
    walk stack (Var n) = IVar (findInd stack n)
    walk stack (Abs n t) = IAbs (walk (n : stack) t)
    walk stack (App t1 t2) = IApp (walk stack t1) (walk stack t2)
    findInd stack n =
      case (n `L.elemIndex` stack, n `L.elemIndex` ctx) of
        (Just i, _) -> i
        (Nothing, Just i) -> L.length stack + i
        _ -> error $ "No index for free variable " ++ n

termShift :: Int -> ITerm -> ITerm
termShift d = walk 0
  where
    walk c (IVar x)
      | x >= c = IVar (x + d)
      | otherwise = IVar x
    walk c (IAbs t') = IAbs (walk (c + 1) t')
    walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

termSubst :: Int -> ITerm -> ITerm -> ITerm
termSubst j s = walk 0
  where
    walk c (IVar x)
      | x == j+c = termShift c s
      | otherwise = IVar x
    walk c (IAbs t') = IAbs (walk (c + 1) t')
    walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)

termSubstTop :: ITerm -> ITerm -> ITerm
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: ITerm -> Bool
isVal (IAbs _) = True
isVal _ = False

eval :: ITerm -> ITerm
eval (IApp (IAbs t') v2) | isVal v2 = termSubstTop v2 t'
eval (IApp v1 t2) | isVal v1 = IApp v1 (eval t2)
eval (IApp t1 t2) = IApp (eval t1) t2
eval t = error $ "No rule to apply for: " ++ show t

