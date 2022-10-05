module SimpleBoolSpec
  ( spec
  )
where

import           Control.Monad
import           SimpleBool.Eval
import           SimpleBool.Parse
import           SimpleBool.Syntax
import           SimpleBool.TypeCheck
import           Test.Hspec

exec :: String -> String
exec str = case parseStr str of
  Right terms -> case typeOf newCtx terms of
    Right ty  -> printTm newCtx (eval terms) ++ " : " ++ printType ty
    Left  err -> err
  Left err -> error $ show err

spec :: Spec
spec = do
  describe "eval" $
    forM_ eval_tests $ \(input, expected) ->
      it (printTm newCtx input ++ " => " ++ printTm newCtx expected) $ eval input `shouldBe` expected

  describe "type" $ do
    forM_ type_tests $ \(input, expected) ->
      it (printTm newCtx input ++ " => " ++ printType expected) $ typeOf newCtx input `shouldBe` Right expected

    it "cannot type example"
      $ typeOf newCtx (TmApp(TmAbs "x" TyBool (TmVar 0 1)) (TmAbs "x" TyBool (TmVar 0 1)))
      `shouldBe` Left "parameter type mismatch"

  describe "parse & eval" $ do
    forM_ parse_eval_tests $ \(input, expected) ->
      it (input ++ " => " ++ expected) $ exec input `shouldBe` expected
 where
  eval_tests =
    [
      (TmAbs "x" TyBool (TmVar 0 1), TmAbs "x" TyBool (TmVar 0 1))
    -- (λx: Bool.x) => (λx: Bool.x)
    , (TmApp (TmAbs "x" (TyArr TyBool TyBool) (TmVar 0 1)) (TmAbs "x" TyBool (TmVar 0 1)), TmAbs "x" TyBool (TmVar 0 1))
    -- (λx: Bool->Bool .x)(λx: Bool.x)
    -- => (λx: Bool.x)
    , (TmApp (TmAbs "x" TyBool (TmVar 0 1)) (TmAbs "x" TyBool (TmVar 0 1)), TmAbs "x" TyBool (TmVar 0 1))
    -- (λx: Bool .x)(λx: Bool.x) <- cannnot type
    , (TmApp(TmAbs "x" TyBool TmTrue) TmFalse, TmTrue)
    -- (λx: Bool.true) false => true
    ]
  type_tests =
    [
      (TmAbs "x" TyBool (TmVar 0 1), TyArr TyBool TyBool)
    -- (λx: Bool.x) : Bool -> Bool
    , (TmApp(TmAbs "x" (TyArr TyBool TyBool) (TmVar 0 1)) (TmAbs "x" TyBool (TmVar 0 1)), TyArr TyBool TyBool)
    -- (λx: Bool->Bool .x)(λx: Bool.x) : Bool -> Bool
    , (TmApp(TmAbs "x" TyBool TmTrue) TmFalse, TyBool)
    -- (λx: Bool.true) false : Bool
    ]
  parse_eval_tests =
    [ ("(λx: Bool.x)"                           , "(λx.x) : Bool -> Bool")
    , ("(λx: Bool -> Bool.x)(λx:Bool.x)"        , "(λx.x) : Bool -> Bool")
    , ("(λx: Bool.x)(λx:Bool.x)"                , "parameter type mismatch")
    , ("(λx: Bool.true)"                        , "(λx.true) : Bool -> Bool")
    , ("(λx: Bool.true) false"                  , "true : Bool")
    , ("(λx: Bool. if x then true else x) false", "false : Bool")
    ]
