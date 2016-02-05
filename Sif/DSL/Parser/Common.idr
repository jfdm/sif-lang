-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Sif Specific Parsing Utils
module Sif.DSL.Parser.Common

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Sif.DSL.Parser.Utils

import Test.Parsing

%access export

literal : Parser String
literal = do
    token "\"\"\""
    ss <- manyTill anyChar (token "\"\"\"")
    pure (pack ss)

sifComment : Parser ()
sifComment = comment "--" "{-" "-}" <?> "Sif Comment"

sifDoc : Parser String
sifDoc = doc ">"
    <?> "Sif Doc String"

descString : Parser String
descString = literal
         <|> (quoted '"')
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
     spaces
     pure s
    <?> "Description"

ident : Parser String
ident = do
    i <- (map pack $ some alphaNum)
    spaces
    pure i
  <?> "Identity"

title : Parser String
title = do
   t <- (quoted '"')
   spaces
   pure t
  <?> "Title"

runTests : IO ()
runTests = do
  putStrLn $ heading "Parsing Common Tests"
  canParse (Just "Literal")     literal      "\"\"\" I am a literal\"\"\""
  canParse (Just "Desc String") descString   "\"\"\" I am a literal\"\"\""
  canParse (Just "Desc String") descString   "\"aaaaaa\""
  canParse (Just "Comments")    (sifComment) """-- I am a comment\n"""
  canParse (Just "Comments")    (sifComment) """{- I am a comment -}"""
  canParse (Just "Doc Stringd") sifDoc       "> I am a doc\n"

  parseTestG (Just "Desc")      (desc) "Description \"I am\"" "I am" (==)

  parseTestG (Just "Titles")
             title
             "\"I am a title\""
             "I am a title"
             (==)
  parseTestG (Just "Identifier")
             ident
             "ident"
             "ident"
             (==)


-- --------------------------------------------------------------------- [ EOF ]
