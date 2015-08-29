-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Things that should be in Lightyear but are Not.
module Sif.DSL.Parser.Utils

import Lightyear
import Lightyear.Strings

%default partial
%access public

eol : Parser Char
eol = char '\n'

indent1 : Parser ()
indent1 = skip $ string "  "

indent2 : Parser ()
indent2 = indent1 *> indent1

indent3 : Parser ()
indent3 = indent2 *> indent1


anyChar : Parser Char
anyChar = satisfy (const True)

literallyBetweenLR : Char -> Char -> Parser String
literallyBetweenLR l r =
    map pack $ between (lexeme $ char l) (lexeme $ char r) (some (satisfy (/= r)))

literallyBetween : Char -> Parser String
literallyBetween c = literallyBetweenLR c c

manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan : Monad m => ParserT m String (List a)
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}

literal : Parser String
literal = do
    token "\"\"\""
    ss <- manyTill anyChar (token "\"\"\"")
    pure (pack ss)

-- --------------------------------------------------------------------- [ EOF ]
