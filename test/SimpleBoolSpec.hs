module SimpleBoolSpec
  ( spec
  )
where

import           Test.Hspec
import           SimpleBool.Syntax
import           SimpleBool.Eval
import           SimpleBool.Parse

exec :: String -> String
exec str = case parseStr str of
  Right terms -> case typeOf newCtx terms of
    Right ty  -> (printTm newCtx $ eval terms) ++ " : " ++ (printType ty)
    Left  err -> err
  Left err -> error $ show err

spec :: Spec
spec = describe "exec" $ do
  it "eval (λx: Bool.x) => (λx: Bool.x) : Bool -> Bool"
    $          exec "(λx: Bool.x)"
    `shouldBe` "(λx.x) : Bool -> Bool"

  it "eval (λx: Bool -> Bool.x)(λx:Bool.x) => (λx.x) : Bool -> Bool"
    $          exec "(λx: Bool -> Bool.x)(λx:Bool.x)"
    `shouldBe` "(λx.x) : Bool -> Bool"

  it "eval (λx: Bool.x)(λx:Bool.x) => parameter type mismatch"
    $          exec "(λx: Bool.x)(λx:Bool.x)"
    `shouldBe` "parameter type mismatch"

  it "eval (λx: Bool.true) => (λx: Bool.x) : Bool -> Bool"
    $          exec "(λx: Bool.true)"
    `shouldBe` "(λx.true) : Bool -> Bool"

  it "eval (λx: Bool.true) false => true : Bool"
    $          exec "(λx: Bool.true) false"
    `shouldBe` "true : Bool"

  it "eval (λx: Bool. if x then true else x) false => false : Bool"
    $          exec "(λx: Bool. if x then true else x) false"
    `shouldBe` "false : Bool"
