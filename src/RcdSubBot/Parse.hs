module RcdSubBot.Parse
  -- ( parseStr
  -- )
where

import           Data.Functor.Identity
import           Data.Void
import           RcdSubBot.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

-- type Parser = Parsec Void String

-- parseStr :: String -> Either (ParseErrorBundle String Void) Term
-- parseStr = parse (parseTerm newCtx) ""

-- sc :: Parser ()
-- sc = L.space space1 (L.skipLineComment "--") empty

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

-- symbol :: String -> Parser String
-- symbol = L.symbol sc

-- keywords = ["true", "false", "if", "then", "else", "Bool", "λ"]

-- parseTrue :: Parser Term
-- parseTrue = symbol "true" >> return TmTrue

-- parseFalse :: Parser Term
-- parseFalse = symbol "false" >> return TmFalse

-- parseIf :: Context -> Parser Term
-- parseIf ctx = do
--   symbol "if"
--   t1 <- parseTerm ctx
--   symbol "then"
--   t2 <- parseTerm ctx
--   symbol "else"
--   TmIf t1 t2 <$> parseTerm ctx

-- parseBrackets :: Context -> Parser Term
-- parseBrackets ctx = do
--   symbol "("
--   term <- parseTerm ctx
--   symbol ")"
--   return term

-- parseVarName :: Parser String
-- parseVarName = do
--   let parse = lexeme $ some alphaNumChar
--   var <- lookAhead $ parse
--   if var `elem` keywords then fail "conflict keywords" else parse

-- parseVar :: Context -> Parser Term
-- parseVar ctx = do
--   var <- parseVarName
--   idx <- case getVarIndex ctx var of
--     Just i  -> return i
--     Nothing -> error ("cannot find variable " ++ var)
--   return $ TmVar idx (length ctx)

-- parseType :: Parser Ty
-- parseType =
--   symbol "Bool" >> (symbol "->" >> TyArr TyBool <$> parseType) <|> return TyBool

-- parseAbs :: Context -> Parser Term
-- parseAbs ctx = do
--   symbol "λ"
--   var <- parseVarName
--   symbol ":"
--   ty <- parseType
--   symbol "."
--   term <- parseTerm $ bindVarName var ctx
--   return $ TmAbs var ty term

-- parseTerm :: Context -> Parser Term
-- parseTerm ctx = do
--   t1 <- choice
--     [ parseTrue
--     , parseFalse
--     , parseIf ctx
--     , parseBrackets ctx
--     , parseAbs ctx
--     , parseVar ctx
--     ]
--   (TmApp t1 <$> parseTerm ctx) <|> return t1
