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


comment : String -> String -> String -> Parser ()
comment l b e = (line l)
            <|> (block b e)
            <|> space
            <?> "Comment"
    where
      line : String -> Parser ()
      line l = do
          token l
          manyTill anyChar eol
          space
          pure ()
        <?> "Line comment"

      block : String -> String -> Parser ()
      block b e = do
          token b
          manyTill anyChar (token e)
          space
          pure ()
        <?> "Block Comment"

doc : String -> Parser String
doc m = do
      ds <- some $ docString m
      space
      pure $ concat ds
    <?> "Documentation"
  where
      docString : String -> Parser String
      docString m = do
          token m
          d <- manyTill anyChar eol
          space
          pure $ pack d
        <?> "Doc String"

-- --------------------------------------------------------------------- [ EOF ]
