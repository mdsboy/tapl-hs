module ArithSpec
  ( spec
  )
where

import           Arith.Eval
import           Arith.Parse
import           Arith.Syntax
import           Control.Monad
import           Test.Hspec

exec :: String -> Term
exec str = case parseStr str of
  Right terms -> eval terms
  Left  err   -> error $ show err

spec = describe "exec" $ do
  forM_ tests $ \(input, expected) ->
    it (input ++ " => " ++ printTm expected) $ exec input `shouldBe` expected
  it "nesting if example"
    $          exec
                 "if (if true then false else true) then (if true then 0 else 0) else (if true then (succ 0) else (succ pred 0))"
    `shouldBe` TmSucc TmZero
 where
  tests =
    [ ("iszero 0"                          , TmTrue)
    , ("iszero (succ 0)"                   , TmFalse)
    , ("iszero (pred succ 0)"              , TmTrue)
    , ("if (iszero 0) then true else false", TmTrue)
    , ("if (iszero 0) then (succ 0) else 0", TmSucc TmZero)
    ]
