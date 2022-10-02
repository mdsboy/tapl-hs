module Arith.Syntax
  ( Term(..)
  , printTm
  )
where

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving(Eq,Show)

printTm :: Term -> String
printTm TmTrue  = "true"
printTm TmFalse = "false"
printTm (TmIf t1 t2 t3) =
  "if " ++ printTm t1 ++ " then " ++ printTm t2 ++ " else " ++ printTm t3
printTm TmZero       = "0"
printTm (TmSucc   t) = "(succ " ++ printTm t ++ ")"
printTm (TmPred   t) = "(pred " ++ printTm t ++ ")"
printTm (TmIsZero t) = "(iszero " ++ printTm t ++ ")"
