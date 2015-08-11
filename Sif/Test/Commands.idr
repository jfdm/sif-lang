module Sif.Test.Commands

import Lightyear
import Lightyear.Strings

import Sif.Parser.Common
import Sif.Parser.Utils

import Sif.Pattern
import Sif.Commands

import Sif.Test.Harness

parseTest : Show a => String
                   -> (String -> Either String a)
                   -> String
                   -> a
                   -> (a -> a -> Bool)
                   -> IO ()
parseTest title p inStr exp eq = do
  putStrLn $ unwords ["Begin Test:", title]
  case p inStr of
    Left err  => with List do
         putStrLn $ unlines [
               errLine
             , "Unexpected Parse Error:\n"
             , err
             , errLine
             ]
    Right res => do
      if eq res exp
        then return ()
        else with List
           Prelude.putStrLn $ unlines [
                 errLine
               , "Error:\n"
               , "Given:"
               , "\t" ++ show inStr
               , "Expected:"
               , "\t" ++ show exp
               , errLine
               ]


testChkPattern : IO ()
testChkPattern = parseTest
    "Parse :chkPattern"
    parseCMD
    ":chkPattern \"problem.p.sif\" \"solution.s.sif\""
    (CheckExtPattern "problem.p.sif" "solution.s.sif")
    (==)

testQuit : IO ()
testQuit = parseTest
    "Parse :q"
    parseCMD
    ":q"
    (Quit)
    (==)

runTests : IO ()
runTests = do
  putStrLn "Testing Command Parsing"
  putStrLn infoLine

  testChkPattern
  testQuit
