module SimpleBool.Eval
  ( eval
  )
where

import           SimpleBool.Syntax

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

eval1 :: Term -> Maybe Term
eval1 TmTrue = Nothing
eval1 TmFalse = Nothing
eval1 (TmIf TmTrue t2 _) = return t2
eval1 (TmIf TmFalse _ t3) = return t3
eval1 (TmIf t1 t2 t3) = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1
eval1 (TmApp (TmAbs x ty t12) v2) | isVal v2 = return (termSubstTop v2 t12)
eval1 (TmApp v1 t2) | isVal v1 = TmApp v1 <$> eval1 t2
eval1 (TmApp t1 t2) = (`TmApp` t2) <$> eval1 t1
eval1 _ = Nothing

isVal :: Term -> Bool
isVal TmTrue   = True
isVal TmFalse  = True
isVal TmAbs {} = True
isVal t        = False

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

termShift :: Int -> Term -> Term
termShift d = termShiftRec d 0

termShiftRec :: Int -> Int -> Term -> Term
termShiftRec d = tmmap f
 where
  f :: Int -> Int -> Int -> Term
  f c x n | x >= c    = TmVar (x + d) (n + d)
          | otherwise = TmVar x (n + d)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = tmmap f 0
 where
  f :: Int -> Int -> Int -> Term
  f c x n | x == j + c = termShift c s
          | otherwise  = TmVar x n

tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar c (TmVar x n   ) = onvar c x n
tmmap onvar c (TmAbs x ty t) = TmAbs x ty (tmmap onvar (c + 1) t)
tmmap onvar c (TmApp t1 t2 ) = TmApp (tmmap onvar c t1) (tmmap onvar c t2)
tmmap onvar c (TmIf t1 t2 t3) =
  TmIf (tmmap onvar c t1) (tmmap onvar c t2) (tmmap onvar c t3)
tmmap onvar c t = t
