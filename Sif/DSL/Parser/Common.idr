-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Sif Specific Parsing Utils
module Sif.DSL.Parser.Common

import Lightyear
import Lightyear.Strings

import Sif.DSL.Parser.Utils

descString : Parser String
descString = literal
         <|> (literallyBetween '\"')
         <|> (literallyBetweenLR '{' '}')
         <?> "Description Block"

keyword : String -> Parser ()
keyword s = string s *> space *> pure ()

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
