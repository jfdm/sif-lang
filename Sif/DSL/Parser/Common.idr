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

sifComment : Parser ()
sifComment = comment "--" "{-" "-}" <?> "Sif Comment"

sifDoc : Parser String
sifDoc = doc ">"
    <?> "Sif Doc String"

descString : Parser String
descString = literal
         <|> (literallyBetween '\"')
         <|> (literallyBetweenLR '{' '}')
         <?> "Description Block"

keyword : String -> Parser ()
keyword s = do
     string s
     sifComment
     pure ()

desc : Parser $ String
desc = do
     token "Description"
     s <- descString
     space
     pure s
    <?> "Description"

ident : Parser String
ident = lexeme (map pack $ some (satisfy isAlphaNum) ) <?> "Identity"

title : Parser String
title = literallyBetween '"'  <?> "Title"


-- --------------------------------------------------------------------- [ EOF ]
