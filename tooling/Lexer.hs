module Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (GenParser)
import qualified Text.Parsec.Token as Tok
-- import Control.Monad.Identity

import Text.Parsec.Language (haskellStyle)
import Model

type Parser a = GenParser Char [Pattern] a
-- ----------------------------------------------------- [ Define Token Parser ]

--lexer :: Tok.TokenParser [Pattern]
lexer = Tok.makeTokenParser style
    where ops = ["<-", ":"]
          names = ["linkedTo", "uses",                               -- Relations
                   ":ofType", ":extends", ":implements",             -- Properties
                   "Pattern", "Abstract", "Integration", "language", -- 'Types'
                   "from", "import", "relations", "patterns"]  -- Keywords
          style = haskellStyle {Tok.reservedOpNames = ops,
                                Tok.reservedNames = names,
                                Tok.commentLine = "--"}

-- ------------------------------------------------------ [ Define Lexer Rules ]

-- Core Lexer Operation

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

comma :: Parser ()
comma = void (Tok.comma lexer)

-- [ <- These bad boys
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- ( <- These base boys
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- { <- These bad boys
braces :: Parser a -> Parser a
braces = Tok.braces lexer

-- Reserved Words
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- Reserved Operations
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- Identifiers
identifier :: Parser String
identifier = Tok.identifier lexer

-- Strings
stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

runLex :: Parser a -> Parser a
runLex p = do
  Tok.whiteSpace lexer
  res <- p
  eof
  return res
                      

-- --------------------------------------------------------------------- [ EOF ]
