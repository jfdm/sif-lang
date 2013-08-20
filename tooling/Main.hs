module Main (main) where

import System.Environment (getArgs)

import Text.Parsec.String (Parser)
import Text.Parsec.String (parseFromFile)
import Parser (parsePatternLang)
import Lexer (runLex)
import Model

main = do 
  a <- getArgs
  res <- parseFromFile (runLex parsePatternLang) (head a)
  case res of
    Left err -> error (show err)
    Right x -> print x
           
