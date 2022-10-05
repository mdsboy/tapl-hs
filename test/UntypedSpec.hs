module UntypedSpec
  ( spec
  )
where

import           Control.Monad
import           Test.Hspec
import           Untyped.Eval
import           Untyped.Parse
import           Untyped.Syntax


exec :: String -> String
exec str = case parseStr str of
  Right terms -> printTm newCtx $ eval terms
  Left  err   -> error $ show err

spec :: Spec
spec = do
  describe "eval" $
    forM_ eval_tests $ \(input, expected) ->
      it (printTm newCtx input ++ " => " ++ printTm newCtx expected) $ eval input `shouldBe` expected

  describe "parse & eval" $ do
    forM_ parse_eval_tests $ \(input, expected) ->
      it (input ++ " => " ++ expected) $ exec input `shouldBe` expected

 where
  eval_tests =
    [
      (TmAbs "x" (TmVar 0 1), TmAbs "x" (TmVar 0 1))
    -- (λx.x)
    , (TmApp (TmAbs "x" (TmVar 0 1)) (TmAbs "x" (TmVar 0 1)), TmAbs "x" (TmVar 0 1))
    -- (λx.x)(λx.x)
    , (TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar 0 2) (TmVar 1 2))))(TmAbs "a" (TmVar 0 1)),  TmAbs "y" (TmApp (TmVar 0 1) (TmAbs "a" (TmVar 0 2))))
    -- (λx.(λy.y x))(λa.a)
    ]
  parse_eval_tests =
    [ ("(λx.x)"             , "(λx.x)")
    , ("(λx.x)(λx.x)"       , "(λx.x)")
    , ("(λx.(λy.y x))(λa.a)", "(λy.(y (λa.a)))")
    ]
