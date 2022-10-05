module SimpleBool.TypeCheck
  ( typeOf
  )
where

import           SimpleBool.Syntax

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
typeOf ctx (TmIf t1 t2 t3) = if typeOf ctx t1 == Right TyBool
  then
    let tyT2 = typeOf ctx t2
        tyT3 = typeOf ctx t3
    in  case tyT2 of
          Right _ -> if tyT2 == tyT3
            then tyT2
            else Left "arms of conditional have different types"
          Left err -> Left err
  else Left "guard of conditional not a boolean"


