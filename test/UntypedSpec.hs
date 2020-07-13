module UntypedSpec
  ( spec
  )
where

import           Test.Hspec
import           Untyped.Syntax
import           Untyped.Eval
import           Untyped.Parse
import           Control.Monad

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
