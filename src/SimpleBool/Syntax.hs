module SimpleBool.Syntax
  ( Term(..)
  , Ty(..)
  , Context
  , Binding(..)
  , newCtx
  , bindVarName
  , getVarIndex
  , addBinding
  , getTypeFromContext
  , printTm
  , printType
  )
where

import           Data.List

data Ty =
    TyArr Ty Ty
  | TyBool
  deriving(Eq, Show)

data Term =
    TmVar Int Int
  | TmAbs String Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving(Eq, Show)

data Binding =
  NameBind
  | VarBind Ty
  deriving(Eq, Show)

type Context = [(String, Binding)]

------------------------   Printing  ------------------------

printTm :: Context -> Term -> String
printTm ctx (TmAbs x ty t1) = "(λ" ++ x' ++ "." ++ printTm ctx' t1 ++ ")"
  where (ctx', x') = pickFreshName ctx x
printTm ctx (TmApp t1 t2) =
  "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
printTm ctx (TmVar x n) | length ctx == n = indexToName ctx x
                        | otherwise       = "bad index"
printTm ctx TmTrue  = "true"
printTm ctx TmFalse = "false"
printTm ctx (TmIf t1 t2 t3) =
  "if "
    ++ printTm ctx t1
    ++ " then "
    ++ printTm ctx t2
    ++ " else "
    ++ printTm ctx t3

printType :: Ty -> String
printType (TyArr ty1 ty2) = printType ty1 ++ " -> " ++ printType ty2
printType TyBool          = "Bool"

------------------------   Context management  ------------------------

newCtx :: Context
newCtx = []

bindVarName :: String -> Context -> Context
bindVarName varname ctx = (varname, NameBind) : ctx

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x = (ctx', x')
 where
  f :: Context -> String -> String
  f [] x = x
  f ((c, _) : cs) x | x == c    = f cs (x ++ "'")
                    | otherwise = f cs x
  x'   = f ctx x
  ctx' = bindVarName x' ctx

indexToName :: Context -> Int -> String
indexToName ctx x = fst $ ctx !! x

getVarIndex :: Context -> String -> Maybe Int
getVarIndex ctx var = elemIndex (var, NameBind) ctx

getBinding :: Context -> Int -> Binding
getBinding ctx x = snd $ ctx !! x

getTypeFromContext :: Context -> Int -> Either String Ty
getTypeFromContext ctx i = case getBinding ctx i of
  (VarBind ty) -> Right ty
  _            -> Left
    (  "getTypeFromContext: Wrong kind of binding for variable "
    ++ indexToName ctx i
    )
