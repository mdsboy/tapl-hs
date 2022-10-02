module Untyped.Parse
  ( parseStr
  )
where

import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Untyped.Syntax

type Parser = Parsec Void String

parseStr :: String -> Either (ParseErrorBundle String Void) Term
parseStr = parse (parseTerm newCtx) ""

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseBrackets :: Context -> Parser Term
parseBrackets ctx = do
  symbol "("
  term <- parseTerm ctx
  symbol ")"
  return term

parseVar :: Context -> Parser Term
parseVar ctx = do
  var <- lexeme $ some alphaNumChar
  idx <- case getVarIndex ctx var of
    Just i  -> return i
    Nothing -> error ("cannot find variable " ++ var)
  return $ TmVar idx (length ctx)

parseAbs :: Context -> Parser Term
parseAbs ctx = do
  symbol "λ"
  var <- lexeme $ some alphaNumChar
  symbol "."
  term <- parseTerm $ bindVarName var ctx
  return $ TmAbs var term

parseTerm :: Context -> Parser Term
parseTerm ctx = do
  t1 <- choice [parseBrackets ctx, parseAbs ctx, parseVar ctx]
  (TmApp t1 <$> parseTerm ctx) <|> return t1
