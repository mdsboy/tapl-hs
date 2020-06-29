module Untyped
    ( Term(..)
    , eval
    )
where

data Term =
    TmVar Int
  | TmAbs String Term
  | TmApp Term Term
  deriving(Eq, Show)

termShift :: Int -> Term -> Term
termShift d = termShiftRec d 0

termShiftRec :: Int -> Int -> Term -> Term
termShiftRec d = tmmap f
  where
    f :: Int -> Int -> Term
    f c x | x >= c    = (TmVar (x + d))
          | otherwise = (TmVar x)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = tmmap f 0
  where
    f :: Int -> Int -> Term
    f c x | x == j + c = (termShift c s)
          | otherwise  = (TmVar x)

tmmap :: (Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c (TmVar x    ) = onvar c x
tmmap onvar c (TmAbs x  t ) = (TmAbs x (tmmap onvar (c + 1) t))
tmmap onvar c (TmApp t1 t2) = (TmApp (tmmap onvar c t1) (tmmap onvar c t2))

eval :: Term -> Term
eval t = case eval1 t of
    Just t' -> eval t'
    Nothing -> t

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs x t12) v2) | isVal v2 = return (termSubstTop v2 t12)
eval1 (TmApp v1 t2) | isVal v1 = (\t2' -> (TmApp v1 t2')) <$> eval1 t2
eval1 (TmApp t1 t2) = (\t1' -> (TmApp t1' t2)) <$> eval1 t1
eval1 _ = Nothing

isVal :: Term -> Bool
isVal (TmAbs _ _) = True
isVal _           = False
