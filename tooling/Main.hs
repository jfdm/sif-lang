module Main (main) where

import System.Environment (getArgs)
import System.IO
import Parser
import Model


main :: IO ()
main = do args <- getArgs 
          content <- readFile (args !! 0)
          let ast = parseSif content
          print ast

          
