module Tokenizer where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.String (Parser)

-- Define the types of tokens we want to parse
data Token = Identifier String | Keyword String | Operator String | Literal String deriving (Show)

-- Define the C language definition for the token parser
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

-- Create the C token parser using the language definition
cTokenParser :: Token.TokenParser ()
cTokenParser = Token.makeTokenParser cDef

-- Parse an identifier token
parseIdentifier :: Parser Token
parseIdentifier = do
  name <- Token.identifier cTokenParser
  return $ Identifier name

-- Parse a keyword token
parseKeyword :: Parser Token
parseKeyword = do
  name <- Token.identifier cTokenParser
  return $ Keyword name

-- Parse an operator token
parseOperator :: Parser Token
parseOperator = do
  op <- Token.operator cTokenParser
  return $ Operator op

-- Parse a literal token
parseLiteral :: Parser Token
parseLiteral = do
  lit <- Token.stringLiteral cTokenParser <|> (show <$> Token.integer cTokenParser)
  return $ Literal lit

-- Parse a single token
parseToken :: Parser Token
parseToken = choice [parseIdentifier, parseKeyword, parseOperator, parseLiteral]

-- Parse a list of tokens

parseTokens :: String -> Either ParseError [Token]
parseTokens input = parse (many parseToken) "" input

