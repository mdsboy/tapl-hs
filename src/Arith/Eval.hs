module Arith.Eval
    ( eval
    , Term(..)
    )
where

import           Arith.Syntax

eval :: Term -> Term
eval t = case eval1 t of
    Just t' -> eval t'
    Nothing -> t

eval1 :: Term -> Maybe Term
eval1 TmTrue                                   = Nothing
eval1 TmFalse                                  = Nothing
eval1 (TmIf TmTrue  t2 _ )                     = return t2
eval1 (TmIf TmFalse _  t3)                     = return t3
eval1 (TmIf t1 t2 t3) = (\t1' -> TmIf t1' t2 t3) <$> eval1 t1
eval1 TmZero                                   = Nothing
eval1 (TmSucc t     )                          = TmSucc <$> eval1 t
eval1 (TmPred TmZero)                          = Nothing
eval1 (TmPred (TmSucc nv)) | isNumericVal nv   = return nv
eval1 (TmPred   t     )                        = TmPred <$> eval1 t
eval1 (TmIsZero TmZero)                        = return TmTrue
eval1 (TmIsZero (TmSucc nv)) | isNumericVal nv = return TmFalse
eval1 (TmIsZero t)                             = TmIsZero <$> eval1 t

isNumericVal :: Term -> Bool
isNumericVal TmZero      = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _           = False

isVal :: Term -> Bool
isVal TmTrue  = True
isVal TmFalse = True
isVal t       = isNumericVal t
