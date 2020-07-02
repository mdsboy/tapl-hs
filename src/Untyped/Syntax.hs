module Untyped.Syntax
    ( Term(..)
    , Context
    , newCtx
    , bindVarName
    , showTm
    )
where

data Term =
    TmVar Int Int
  | TmAbs String Term
  | TmApp Term Term
  deriving(Eq, Show)

type Context = [String]

newCtx :: Context
newCtx = []

showTm :: Context -> Term -> String
showTm ctx (TmAbs x t1) = "(λ" ++ x' ++ "." ++ (showTm ctx' t1) ++ ")"
    where (ctx', x') = pickFreshName ctx x
showTm ctx (TmApp t1 t2) =
    "(" ++ (showTm ctx t1) ++ " " ++ (showTm ctx t2) ++ ")"
showTm ctx (TmVar x n) | length ctx == n = indexToName ctx x
                       | otherwise       = "bad index"

bindVarName :: String -> Context -> Context
bindVarName varname ctx = varname : ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x = (x' : ctx, x')
  where
    f :: Context -> String -> String
    f [] x = x
    f (c : cs) x | x == c    = f cs (x ++ "'")
                 | otherwise = f cs x
    x' = f ctx x

indexToName :: Context -> Int -> String
indexToName ctx x = ctx !! x
