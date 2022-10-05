{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RcdSubBot.TypeCheck
  ( typeOf
  )
where

import           RcdSubBot.Syntax

------------------------   TYPING  ------------------------

typeOf :: Context -> Term -> Either String Ty
typeOf ctx (TmRecord r) = do
  strty <- getRecordTy r
  return $ TyRecord strty
  where
    getRecordTy :: [(String, Term)] -> Either String [(String, Ty)]
    getRecordTy [] = Right []
    getRecordTy ((str, tm):xs) = do
      ty <- typeOf ctx tm
      strtys <- getRecordTy xs
      return $ (str, ty) : strtys
typeOf ctx (TmProj t str) = case typeOf ctx t of
  Right (TyRecord r) -> case lookup str r of
    Just ty1 -> Right ty1
    Nothing  -> Left ("label " ++ str ++ " not found")
  Right TyBot -> Right TyBot
  Right _ -> Left "Expected record type"
  Left err -> Left err
typeOf ctx (TmVar x n       ) = getTypeFromContext ctx x
typeOf ctx (TmAbs t1 tyT1 t2) = case typeOf ctx' t2 of
  Right tyT2 -> Right $ TyArr tyT1 tyT2
  Left  err  -> Left err
 where
  ctx' = addBinding ctx t1 (VarBind tyT1)
typeOf ctx (TmApp t1 t2) = case typeOf ctx t1 of
  Right (TyArr tyT11 tyT12) -> case typeOf ctx t2 of
    Right tyT2 | subType tyT2 tyT11 -> Right tyT12
    _                               -> Left "parameter type mismatch"
  Right _   -> Left "arrow type expected"
  Left  err -> Left err

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
