module UntypedSpec
    ( spec
    )
where

import           Test.Hspec
import           Untyped

spec :: Spec
spec = describe "eval" $ do
    it "eval (λx.x) => (λx.x)"
        $          eval (TmAbs "x" (TmVar 0))
        `shouldBe` (TmAbs "x" (TmVar 0))

    it "eval (λx.x)(λx.x) => (λx.x)"
        $          eval (TmApp (TmAbs "x" (TmVar 0)) (TmAbs "x" (TmVar 0)))
        `shouldBe` (TmAbs "x" (TmVar 0))

    it "eval (λx.(λy.yx))(λa.a) => (λy.y(λa.a))"
        $          eval
                       (TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar 0) (TmVar 1))))
                              (TmAbs "a" (TmVar 0))
                       )
        `shouldBe` (TmAbs "y" (TmApp (TmVar 0) (TmAbs "a" (TmVar 0))))
