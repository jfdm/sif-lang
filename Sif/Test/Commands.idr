-- ------------------------------------------------------ [ Commands.idr<Test> ]
-- Module    : Commands.idr<Test>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Test.Commands

import Lightyear
import Lightyear.Strings

import Sif.Parser.Common
import Sif.Parser.Utils

import Sif.Pattern
import Sif.Commands

import Test.Utils
import Test.Parsing

testChkPattern : IO ()
testChkPattern = do
    parseTestG (Just "Parse :chkPattern") cmd
        ":chkPattern \"problem.p.sif\" \"solution.s.sif\""
        (CheckExtPattern "problem.p.sif" "solution.s.sif")
        (==)

testHelp : IO ()
testHelp = do
  putStrLn $ heading "Testing Help Command"
  parseTestG Nothing cmd ":?"    (Help) (==)
  parseTestG Nothing cmd ":help" (Help) (==)

testEvalPatt : IO ()
testEvalPatt = do
  putStrLn $ heading "Testing Eval Pattern"
  parseTestG Nothing cmd ":eval 1" (EvalPattern 1) (==)

testQuit : IO ()
testQuit = do
  putStrLn $ heading "Testing Quit Command"
  parseTestG Nothing cmd ":q" (Quit) (==)
  parseTestG Nothing cmd ":quit" (Quit) (==)
  parseTestG Nothing cmd ":exit" (Quit) (==)

testSave : IO ()
testSave = do
  putStrLn $ heading "Testing Save Command"
  parseTestB Nothing cmd ":saveAs 1 grl"
  parseTestG Nothing cmd
      ":saveAs 1 grl pattern.grl"
      (ShowPattern 1 (Just API.GRL) (Just "pattern.grl")) (==)

testList : IO ()
testList = do
  putStrLn $ heading "Testing List Command"
  parseTestG Nothing cmd ":list" (ListLib) (==)

testShow : IO ()
testShow = do
  putStrLn $ heading "Testing Show"
  parseTestG Nothing cmd ":show 2" (ShowPattern (S (S Z)) Nothing Nothing) (==)
  parseTestG Nothing cmd ":showAs 2 xml"
      (ShowPattern 2 (Just API.XML) Nothing)
      (==)


export
runTests : IO ()
runTests = do
  putStrLn $ heading "Testing Command Parsing"
  testChkPattern
  testQuit
  testHelp
  testEvalPatt
  testSave
  testList
  testShow

-- --------------------------------------------------------------------- [ EOF ]
