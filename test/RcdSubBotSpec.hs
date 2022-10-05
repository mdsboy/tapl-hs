module RcdSubBotSpec
  ( spec
  )
where

import           Control.Monad
import           RcdSubBot.Syntax
import           RcdSubBot.TypeCheck
import           Test.Hspec

-- exec :: String -> String
-- exec str = case parseStr str of
--   Right terms -> case typeOf newCtx terms of
--     Right ty  -> printTm newCtx (eval terms) ++ " : " ++ printType ty
--     Left  err -> err
--   Left err -> error $ show err

spec :: Spec
spec = do
--   describe "eval" $
--     forM_ eval_tests $ \(input, expected) ->
--       it (printTm newCtx input ++ " => " ++ printTm newCtx expected) $ eval input `shouldBe` expected

  describe "type" $
    forM_ type_tests $ \(input, expected) ->
    it (printTm newCtx input ++ " => " ++ printType expected) $ typeOf newCtx input `shouldBe` Right expected

  it "cannot type example"
    -- (λx: Bot->Top .x)(λx: Bot.x)
    -- Bot->Top </: Bot->Bot
    $ typeOf newCtx (TmApp(TmAbs "x" (TyArr TyBot TyTop) (TmVar 0 1)) (TmAbs "x" TyBot (TmVar 0 1)))
    `shouldBe` Left "parameter type mismatch"

--   describe "parse & eval" $ do
--     forM_ parse_eval_tests $ \(input, expected) ->
--       it (input ++ " => " ++ expected) $ exec input `shouldBe` expected
 where
  type_tests =
    [
      (TmAbs "x" TyBot (TmVar 0 1), TyArr TyBot TyBot)
    -- (λx: Bot.x) : Bot -> Bot
    , (TmProj (TmRecord [("lambda", TmAbs "x" TyBot (TmVar 0 1))]) "lambda", TyArr TyBot TyBot)
    -- (λx: {"bot":Bot,"top":Top}.x) : {"bot":Bot,"top":Top}->{"bot":Bot,"top":Top}
    , (TmAbs "x" (TyRecord [("bot", TyBot), ("top", TyTop)]) (TmVar 0 1), TyArr (TyRecord [("bot", TyBot), ("top", TyTop)]) (TyRecord [("bot", TyBot), ("top", TyTop)]))
    -- (λx: {"bot":Bot,"top":Top}.x) : {"bot":Bot,"top":Top}->{"bot":Bot,"top":Top}
    , (TmApp(TmAbs "x" (TyArr TyBot TyBot) (TmVar 0 1)) (TmAbs "x" TyBot (TmVar 0 1)), TyArr TyBot TyBot)
    -- (λx: Bot->Bot .x)(λx: Bot.x) : Bot -> Bot
    , (TmApp(TmAbs "x" (TyArr TyTop TyBot) (TmVar 0 1)) (TmAbs "x" TyBot (TmVar 0 1)), TyArr TyTop TyBot)
    -- (λx: Top->Bot .x)(λx: Bot.x) : Top -> Bot
    -- Top->Bot <: Bot->Bot
    ]
