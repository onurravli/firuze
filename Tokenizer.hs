module Tokenizer where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

data Token = Identifier String | Keyword String | Operator String | Literal String deriving (Show)

cDef :: Token.LanguageDef ()
cDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter <|> char '_'
    , Token.identLetter     = alphaNum <|> char '_'
    , Token.opStart         = Token.opLetter cDef
    , Token.opLetter        = oneOf "+-*/%&|^~!<>="
    , Token.reservedOpNames = ["+", "-", "*", "/", "%", "&", "|", "^", "~", "!", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "++", "--", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<", ">>", "<<=", ">>="]
    , Token.reservedNames   = ["if", "else", "while", "for", "switch", "case", "break", "continue", "return"]
    , Token.caseSensitive   = True
    }

cTokenParser :: Token.TokenParser ()
cTokenParser = Token.makeTokenParser cDef

parseIdentifier :: Parser Token
parseIdentifier = do
  name <- Token.identifier cTokenParser
  return $ Identifier name

parseKeyword :: Parser Token
parseKeyword = do
  name <- Token.identifier cTokenParser
  return $ Keyword name

parseOperator :: Parser Token
parseOperator = do
  op <- Token.operator cTokenParser
  return $ Operator op

parseLiteral :: Parser Token
parseLiteral = do
  lit <- Token.stringLiteral cTokenParser <|> (show <$> Token.integer cTokenParser)
  return $ Literal lit

parseToken :: Parser Token
parseToken = choice [parseIdentifier, parseKeyword, parseOperator, parseLiteral]

parseTokens :: String -> Either ParseError [Token]
parseTokens = parse (many parseToken) ""

