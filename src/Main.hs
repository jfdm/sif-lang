-- | Main Program reads the command line options, checks the supplied
-- sif file, and pushes the dot version out to STDOUT.

module Main (main) where

import System.Environment (getArgs)
import System.IO

import qualified Data.Map as Map
import Parser
import qualified AST
import TypeSystem
import Checker

main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let res = chkPlangSpec $! parseSif content
          print res
          putStrLn " It checks"



-- --------------------------------------------------------------------- [ EOF ]
