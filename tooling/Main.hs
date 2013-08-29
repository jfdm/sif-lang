module Main (main) where

import System.Environment (getArgs)
import System.IO
import Parser
import Model
import Examples
-- import Transform

main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let ast = parseSif content
          print ast
          putStr "\n\n\n"
          --print tropyc
--          putStr $ unlines (plang2Dot ast)


-- --------------------------------------------------------------------- [ EOF ]
