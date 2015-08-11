module Sif.Checker

import System
import Sif.Pattern
import Sif.API
import Sif.Checker.Options
import Sif.Parser

doSyntaxCheck : Maybe String -> Maybe String -> Eff () SifEffs

doSyntaxCheck (Just x) (Just y) = do
    readSifFile problem x
    readSifFile solution y

doSyntaxCheck (Just x) (Nothing) = do
    readSifFile problem x

doSyntaxCheck (Nothing) (Just y) = do
    readSifFile solution y

doSyntaxCheck (Nothing) (Nothing) =
    Sif.raise (FileMissing (unwords ["\nProblem File: ", x, "\nSolution File: ", y ]))

doEvalBuild : Maybe String -> Maybe String -> Eff () SifEffs
doEvalBuild Nothing  Nothing = Sif.raise (FileMissing
    (unwords ["\nProblem File: ", x, "\nSolution File: ", y ]))
doEvalBuild Nothing  _       = Sif.raise (FileMissing ("Solution File "))
doEvalBuild _        Nothing = Sif.raise (FileMissing ("Problem File "))
doEvalBuild (Just p) (Just s) = do
  p <- buildPattern p s
  let eRes = evalPattern p
  printResults eRes


checker : Eff () : SifEffs
checker = do
  os <- 'opts <- get
  if check os
    then do
      doSyntaxCheck (pSpec os) (sSpec os)
    else do
      doBuildEval (pSpec os) (sSpec os)

namespace Main
  main : IO ()
  main = do
    run checker
    exit 0
