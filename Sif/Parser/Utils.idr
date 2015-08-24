module Sif.Parser.Utils

import Lightyear
import Lightyear.Strings

%access public

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
