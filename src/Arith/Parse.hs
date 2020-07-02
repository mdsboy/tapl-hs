module Arith.Parse
  ( parseStr
  )
where

import           Arith.Eval
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void

type Parser = Parsec Void String

parseStr :: String -> Either (ParseErrorBundle String Void) Term
parseStr = parse parseTerm ""

symbol :: String -> Parser String
symbol = L.symbol space

parseTrue :: Parser Term
parseTrue = symbol "true" >> return TmTrue

parseFalse :: Parser Term
parseFalse = symbol "false" >> return TmFalse

parseZero :: Parser Term
parseZero = symbol "0" >> return TmZero

parseSucc :: Parser Term
parseSucc = symbol "succ" >> TmSucc <$> parseTerm

parsePred :: Parser Term
parsePred = symbol "pred" >> TmPred <$> parseTerm

parseIsZero :: Parser Term
parseIsZero = symbol "iszero" >> TmIsZero <$> parseTerm

parseIf :: Parser Term
parseIf = do
  symbol "if"
  t1 <- parseTerm
  symbol "then"
  t2 <- parseTerm
  symbol "else"
  TmIf t1 t2 <$> parseTerm

parseBrackets :: Parser Term
parseBrackets = do
  symbol "("
  t <- parseTerm
  symbol ")"
  return t

parseTerm :: Parser Term
parseTerm = choice
  [ parseTrue
  , parseFalse
  , try parseIf
  , parseZero
  , parseSucc
  , parsePred
  , try parseIsZero
  , parseBrackets
  ]
