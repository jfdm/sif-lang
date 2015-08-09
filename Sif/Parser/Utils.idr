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

-- --------------------------------------------------------------------- [ EOF ]
