-- | The Lexer
module Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (GenParser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (haskellStyle)
import Model.Keywords
import Model.AST

type Parser a = GenParser Char PatternsExpr a
-- ----------------------------------------------------- [ Define Token Parser ]

--lexer :: Tok.TokenParser [Pattern]
lexer = Tok.makeTokenParser style
        where
          style = haskellStyle {Tok.reservedOpNames = sifOperators,
                                Tok.reservedNames = sifKeywords,
                                Tok.commentLine = sifCmtLine}

-- ------------------------------------------------------ [ Define Lexer Rules ]

-- | Do the lexing
runLex :: Parser a -> Parser a
runLex p = do
  Tok.whiteSpace lexer
  res <- p
  eof
  return res

-- | Core Lexer Operation
lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

-- | Lex these bad boys -> ','
comma :: Parser ()
comma = void (Tok.comma lexer)

-- | Lex these bad boys -> '[]'
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- | Lex these base boys -> '()'
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Lex these bad boys -> '{}
braces :: Parser a -> Parser a
braces = Tok.braces lexer

-- | Lex keywords
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Lex operators
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Lex identifiers
identifier :: Parser String
identifier = Tok.identifier lexer

-- | Lex strings
stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

-- --------------------------------------------------------------------- [ EOF ]
