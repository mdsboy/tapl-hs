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
spec = describe "exec" $ forM_ tests $ \(input, expected) ->
  it (input ++ " => " ++ expected) $ exec input `shouldBe` expected
 where
  tests =
    [ ("(λx.x)"             , "(λx.x)")
    , ("(λx.x)(λx.x)"       , "(λx.x)")
    , ("(λx.(λy.y x))(λa.a)", "(λy.(y (λa.a)))")
    ]
