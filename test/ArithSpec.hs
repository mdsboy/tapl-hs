module ArithSpec (spec) where

import           Test.Hspec
import           Arith

spec :: Spec
spec = describe "eval" $ do
  it "eval (iszero 0) => True" $ eval (TmIsZero TmZero) `shouldBe` TmTrue

  it "eval (iszero (succ 0)) => False"
    $          eval (TmIsZero (TmSucc TmZero))
    `shouldBe` TmFalse

  it "eval (iszero (pred (succ 0)) => True"
    $          eval (TmIsZero (TmPred (TmSucc TmZero)))
    `shouldBe` TmTrue

  it "eval (if (iszero 0) True False) => True"
    $          eval (TmIf (TmIsZero TmZero) TmTrue TmFalse)
    `shouldBe` TmTrue

  it "eval (if (iszero (succ 0)) True False) => False"
    $          eval (TmIf (TmIsZero (TmSucc TmZero)) TmTrue TmFalse)
    `shouldBe` TmFalse

  it "eval (if (iszero 0) (succ 0) 0) => True"
    $          eval (TmIf (TmIsZero TmZero) (TmSucc TmZero) TmZero)
    `shouldBe` (TmSucc TmZero)

  it "nesting if example"
    $          eval (TmIf condTerm thenTerm elseTerm)
    `shouldBe` (TmSucc TmZero)
 where
  condTerm = TmIf TmTrue TmFalse TmTrue
  thenTerm = TmIf TmTrue TmZero (TmSucc TmZero)
  elseTerm = TmIf TmTrue (TmSucc TmZero) (TmSucc (TmPred TmZero))
