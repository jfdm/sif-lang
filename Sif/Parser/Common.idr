-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Parser.Common

import Lightyear
import Lightyear.Strings

import Sif.Parser.Utils

descString : Parser String
descString = literal
         <|> (literallyBetween '\"')
         <|> (literallyBetweenLR '{' '}')
         <?> "Description Block"

keyword : String -> Parser ()
keyword s = lexeme $ string s *> pure ()

desc : Parser $ String
desc = do
   lexeme $ string "Description"
   s <- descString
   space
   pure s
  <?> "Description"

ident : Parser String
ident = lexeme (map pack $ some (satisfy isAlphaNum) ) <?> "Identity"

title : Parser String
title = literallyBetween '"' <?> "Title"


-- --------------------------------------------------------------------- [ EOF ]
