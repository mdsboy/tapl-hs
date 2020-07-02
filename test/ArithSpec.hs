module ArithSpec
    ( spec
    )
where

import           Test.Hspec
import           Arith.Syntax
import           Arith.Eval
import           Arith.Parse

exec :: String -> Term
exec str = case parseStr str of
    Right terms -> eval terms
    Left  err   -> error $ show err

spec :: Spec
spec = describe "exec" $ do
    it "exec (iszero 0) => true" $ exec "iszero 0" `shouldBe` TmTrue

    it "exec (iszero (succ 0)) => false"
        $          exec "iszero (succ 0)"
        `shouldBe` TmFalse

    it "eval (iszero (pred (succ 0))) => true"
        $          exec "iszero (pred succ 0)"
        `shouldBe` TmTrue

    it "eval (if (iszero 0) then true else false) => true"
        $          exec "if (iszero 0) then true else false"
        `shouldBe` TmTrue

    it "eval (if (iszero (succ 0)) then true else false) => false"
        $          exec "if (iszero (succ 0)) then true else false"
        `shouldBe` TmFalse

    it "eval (if (iszero 0) then (succ 0) else 0) => (succ 0)"
        $          exec "if (iszero 0) then (succ 0) else 0"
        `shouldBe` (TmSucc TmZero)

    it "nesting if example"
        $          exec
                       "if (if true then false else true) then (if true then 0 else 0) else (if true then (succ 0) else (succ pred 0))"
        `shouldBe` (TmSucc TmZero)
