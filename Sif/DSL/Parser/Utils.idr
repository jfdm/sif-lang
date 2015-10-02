-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Things that should be in Lightyear but are Not.
module Sif.DSL.Parser.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Test.Parsing

%default partial
%access private

line : String -> Parser ()
line l = do
    token l <* spaces
    manyTill anyChar endOfLine
    spaces
    pure ()
  <?> "Line comment"

block : String -> String -> Parser ()
block b e = do
    token b <* spaces
    manyTill anyChar (token e)
    spaces
    pure ()
  <?> "Block Comment"

public
comment : String -> String -> String -> Parser ()
comment l b e = (line l)
            <|> (block b e)
            <|> spaces
            <?> "Comment"

docString : String -> Parser String
docString m = do
    token m <* spaces
    d <- manyTill anyChar endOfLine
    spaces
    pure $ pack d
  <?> "Doc String"

public
doc : String -> Parser String
doc m = do
      ds <- some $ docString m
      spaces
      pure $ concat ds
    <?> "Documentation"

public
runTests : IO ()
runTests = do
  putStrLn $ heading "Parsing Utility Tests"
  canParse (Just "Line")    (line "--")       "-- I am a comment\n"
  canParse (Just "Block")   (block "{-" "-}") "{- -- I am a comment -}"
  canParse (Just "Docs")    (doc ">") """> I am doc
> I am doc
> Doc
>
"""
  canParse (Just "Comment") (comment "--" "{-" "-}") """{-
A comment
-}"""


-- --------------------------------------------------------------------- [ EOF ]
