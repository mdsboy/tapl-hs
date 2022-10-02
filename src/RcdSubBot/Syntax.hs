{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RcdSubBot.Syntax
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
    TyTop
  | TyBot
  | TyArr Ty Ty
  | TyRecord [(String, Ty)]
  deriving(Eq, Show)

data Term =
    TmRecord [(String, Term)]
  | TmProj Term String
  | TmVar Int Int
  | TmAbs String Ty Term
  | TmApp Term Term
  deriving(Eq, Show)

data Binding =
  NameBind
  | VarBind Ty
  deriving(Eq, Show)

type Context = [(String, Binding)]

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

------------------------   Printing  ------------------------

printTm :: Context -> Term -> String
printTm ctx (TmRecord r) = "{" ++ printTmRecord ctx r ++ "}"
  where
    printTmRecord :: Context -> [(String, Term)] -> String
    printTmRecord _ []                 = ""
    printTmRecord ctx ((name, t) : xs) = name ++ ":" ++ printTm ctx t ++ ", " ++ printTmRecord ctx xs
printTm ctx (TmProj t name) = printTm ctx t ++ "." ++ name
printTm ctx (TmAbs x ty t1) = "(λ" ++ x' ++ "." ++ printTm ctx' t1 ++ ")"
  where (ctx', x') = pickFreshName ctx x
printTm ctx (TmApp t1 t2) =
  "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
printTm ctx (TmVar x n) | length ctx == n = indexToName ctx x
                        | otherwise       = "bad index"

printType :: Ty -> String
printType TyTop           = "Top"
printType TyBot           = "Bot"
printType (TyArr ty1 ty2) = printType ty1 ++ " -> " ++ printType ty2
printType (TyRecord r) = "{" ++ printTyRecord r ++ "}"
  where
    printTyRecord :: [(String, Ty)] -> String
    printTyRecord []             = ""
    printTyRecord ((name, t) : xs) = name ++ ":" ++ printType t ++ ", " ++ printTyRecord xs

------------------------   SUBTYPING  ------------------------

-- tyS <: TyT
subType :: Ty -> Ty -> Bool
subType _ TyTop = True
subType TyBot _ = True
-- tyS1 <: tyT1 and tyT2 <: tyS2
-- => (tyS1 -> tyS2) <: (tyT1 -> tyT2)
subType (TyArr tyS1 tyS2) (TyArr tyT1 tyT2) = subType tyS1 tyT1 && subType tyT2 tyS2
-- tySi <: tyTi
subType (TyRecord rS) (TyRecord rT) = subTypeRcdToRcd rS rT
  where
    subTypeRcdToRcd :: [(String, Ty)] -> [(String, Ty)] -> Bool
    subTypeRcdToRcd ((str1, ty1) : xs) ys = case lookup str1 ys of
      Just ty2 -> subType ty1 ty2 && subTypeRcdToRcd xs ys
      Nothing  -> False
subType _ _     = False

------------------------   TYPING  ------------------------

typeOf :: Context -> Term -> Either String Ty
typeof ctx (TmRecord r) = do
  strty <- getRecordTy r
  return $ TyRecord strty
-- typeof ctx (TmRecord r) = getRecordTy r >>= Right . TyRecord
  where
    getRecordTy :: [(String, Term)] -> Either String [(String, Ty)]
    getRecordTy [] = Right []
    getRecordTy ((str, tm):xs) = do
      ty <- typeof ctx tm
      strtys <- getRecordTy xs
      return $ (str, ty) : strtys
typeof ctx (TmProj t str) = case typeOf ctx t of
  Right (TyRecord r) -> case lookup str r of
    Just ty1 -> Right ty1
    Nothing  -> Left ("label " ++ str ++ " not found")
  Right TyBot -> Right TyBot
  Right _ -> Left "Expected record type"
  Left err -> Left err
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

-- typeOf ctx (TmIf t1 t2 t3) = if typeOf ctx t1 == Right TyBool
--   then
--     let tyT2 = typeOf ctx t2
--         tyT3 = typeOf ctx t3
--     in  case tyT2 of
--           Right _ -> if tyT2 == tyT3
--             then tyT2
--             else Left "arms of conditional have different types"
--           Left err -> Left err
--   else Left "guard of conditional not a boolean"
