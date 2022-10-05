module Untyped.Syntax
  ( Term(..)
  , Context
  , newCtx
  , bindVarName
  , printTm
  , getVarIndex
  )
where

import           Data.List

data Term =
    TmVar Int Int
  | TmAbs String Term
  | TmApp Term Term
  deriving(Eq, Show)

type Context = [String]

------------------------   Printing  ------------------------

printTm :: Context -> Term -> String
printTm ctx (TmAbs x t1) = "(λ" ++ x' ++ "." ++ printTm ctx' t1 ++ ")"
  where (ctx', x') = pickFreshName ctx x
printTm ctx (TmApp t1 t2) =
  "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
printTm ctx (TmVar x n) | length ctx == n = indexToName ctx x
                        | otherwise       = "bad index"

------------------------   Context management  ------------------------

newCtx :: Context
newCtx = []

bindVarName :: String -> Context -> Context
bindVarName varname ctx = varname : ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x = (ctx', x')
 where
  f :: Context -> String -> String
  f [] x = x
  f (c : cs) x | x == c    = f cs (x ++ "'")
               | otherwise = f cs x
  x'   = f ctx x
  ctx' = bindVarName x' ctx

indexToName :: Context -> Int -> String
indexToName ctx x = ctx !! x

getVarIndex :: Context -> String -> Maybe Int
getVarIndex ctx var = elemIndex var ctx
