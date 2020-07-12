module SimpleBool.Syntax
  ( Term(..)
  , Ty(..)
  , Context
  , newCtx
  , bindVarName
  , printTm
  , getVarIndex
  , typeOf
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

newCtx :: Context
newCtx = []

printTm :: Context -> Term -> String
printTm ctx (TmAbs x ty t1) = "(λ" ++ x' ++ "." ++ (printTm ctx' t1) ++ ")"
  where (ctx', x') = pickFreshName ctx x
printTm ctx (TmApp t1 t2) =
  "(" ++ (printTm ctx t1) ++ " " ++ (printTm ctx t2) ++ ")"
printTm ctx (TmVar x n) | length ctx == n = indexToName ctx x
                        | otherwise       = "bad index"
printTm ctx TmTrue  = "true"
printTm ctx TmFalse = "false"
printTm ctx (TmIf t1 t2 t3) =
  "if "
    ++ (printTm ctx t1)
    ++ " then "
    ++ (printTm ctx t2)
    ++ " else "
    ++ (printTm ctx t3)

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

printType :: Ty -> String
printType (TyArr ty1 ty2) = (printType ty1) ++ " -> " ++ (printType ty2)
printType TyBool          = "Bool"

typeOf :: Context -> Term -> Either String Ty
typeOf ctx (TmVar x n       ) = getTypeFromContext ctx x
typeOf ctx (TmAbs t1 tyT1 t2) = case tyT2 of
  Right tyT2 -> Right $ TyArr tyT1 tyT2
  Left  err  -> Left err
 where
  ctx' = addBinding ctx t1 (VarBind tyT1)
  tyT2 = typeOf ctx' t2
typeOf ctx (TmApp t1 t2) = case tyT1 of
  Right (TyArr tyT11 tyT12) ->
    if tyT2 == Right tyT11 then Right tyT12 else Left "parameter type mismatch"
  Right _   -> Left "arrow type expected"
  Left  err -> Left err
 where
  tyT1 = typeOf ctx t1
  tyT2 = typeOf ctx t2
typeOf ctx TmTrue          = Right TyBool
typeOf ctx TmFalse         = Right TyBool
typeOf ctx (TmIf t1 t2 t3) = if (typeOf ctx t1) == Right TyBool
  then
    let tyT2 = typeOf ctx t2
        tyT3 = typeOf ctx t3
    in  case tyT2 of
          Right _ -> if tyT2 == tyT3
            then tyT2
            else Left "arms of conditional have different types"
          Left err -> Left err
  else Left "guard of conditional not a boolean"
