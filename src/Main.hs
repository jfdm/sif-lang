-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.

module Main (main) where

import System.Environment (getArgs)
import System.IO
import Parser
import AST
import Pretty.AST

-- import Model
-- import Transform.Dot
-- import Prettifier

main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let ast = parseSif content
          print (prettyPlangAST ast)

--          putStr $ unlines (plang2Dot ast)

-- --------------------------------------------------------------------- [ EOF ]
