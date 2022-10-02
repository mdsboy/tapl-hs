module SimpleBoolSpec
  ( spec
  )
where

import           Control.Monad
import           SimpleBool.Eval
import           SimpleBool.Parse
import           SimpleBool.Syntax
import           Test.Hspec

exec :: String -> String
exec str = case parseStr str of
  Right terms -> case typeOf newCtx terms of
    Right ty  -> printTm newCtx (eval terms) ++ " : " ++ printType ty
    Left  err -> err
  Left err -> error $ show err

spec :: Spec
spec = describe "exec" $ forM_ tests $ \(input, expected) ->
  it (input ++ " => " ++ expected) $ exec input `shouldBe` expected
 where
  tests =
    [ ("(λx: Bool.x)"                           , "(λx.x) : Bool -> Bool")
    , ("(λx: Bool -> Bool.x)(λx:Bool.x)"        , "(λx.x) : Bool -> Bool")
    , ("(λx: Bool.x)(λx:Bool.x)"                , "parameter type mismatch")
    , ("(λx: Bool.true)"                        , "(λx.true) : Bool -> Bool")
    , ("(λx: Bool.true) false"                  , "true : Bool")
    , ("(λx: Bool. if x then true else x) false", "false : Bool")
    ]
