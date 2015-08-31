-- ---------------------------------------------------------------- [ Main.idr ]
-- Module    : Main.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Main

import System

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern
import Sif.Builder.AbsInterp
import Sif.Builder.DirectRep
import Sif.Effs
import Sif.Error
import Sif.Library
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution
import Sif.DSL.Parser
import Sif.DSL
import Sif.Options
import Sif.API
import Sif.REPL

%default partial

runMode : Maybe SifMode -> Eff () SifEffs
runMode Nothing     = putStrLn helpStr
runMode (Just VERS) = printLn FeatureNotImpl
runMode (Just HELP) = putStrLn helpStr

runMode (Just REPL) = do
    loadPrelude
    sifREPL

runMode (Just Eval) = do
    os <- getOptions
    putStrLn $ "Evaluating Pattern"
    evalPatternFromFile (pSpec os) (sSpec os)

runMode (Just Check) = do
    os <- getOptions
    putStrLn $ "Checking Providing Files"
    doSyntaxCheck (pSpec os) (sSpec os)

runMode (Just Conv) = do
    os <- getOptions
    putStrLn "Converting Pattern"
    convPatternFromFile (pSpec os) (sSpec os) (out os) (to os)

covering
sifMain : Eff () SifEffs
sifMain = do
    opts <- parseOptions
    putOptions opts
    setLogLvl (loglvl opts)
    perfSetup
    setSifBackend (backend opts)
    runMode (mode opts)
    displayPerfMetrics

main : IO ()
main = do
  run $ sifMain
  exit 0

-- --------------------------------------------------------------------- [ EOF ]
