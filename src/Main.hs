-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.

module Main (main) where

import System.Environment (getArgs)
import System.IO

import qualified AST as AST
import Parser
import Model
import Checker
import Transform

main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let ast = parseSif content
          let model = chkPlangSpec ast
          let prettyModel = plang2Sif model
          putStrLn $ show (title model) ++ " type checks"
          putStrLn "Caveat: Imports are not checked for now. Sorry!"

-- --------------------------------------------------------------------- [ EOF ]
