
module UntypedSpec
    ( spec
    )
where

import           Test.Hspec
import           Untyped.Syntax
import           Untyped.Eval
import           Untyped.Parse

exec :: String -> String
exec str = case parseStr str of
    Right terms -> showTm newCtx $ eval terms
    Left  err   -> error $ show err

spec :: Spec
spec = describe "exec" $ do
    it "eval (λx.x) => (λx.x)" $ exec "(λx.x)" `shouldBe` "(λx.x)"

    it "eval (λx.x)(λx.x) => (λx.x)" $ exec "(λx.x)(λx.x)" `shouldBe` "(λx.x)"

    it "eval (λx.(λy.y x))(λa.a) => (λy.(y (λa.a)))"
        $          exec "(λx.(λy.y x))(λa.a)"
        `shouldBe` "(λy.(y (λa.a)))"
