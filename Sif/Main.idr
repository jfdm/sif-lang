-- ---------------------------------------------------------------- [ Main.idr ]
-- Module    : Main.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Main

import System.

import Sif.Parser
import Sif.Pattern
import Sif.API
import Sif.REPL
import Sif.Options
import Sif.Effs
import Sif.Error
import Sif.Library

runMode : Maybe SifMode -> Eff () SifEffs
runMode Nothing     = sifREPL
runMode (Just VERS) = printLn FeatureNotImpl
runMode (Just HELP) = putStrLn helpStr

runMode (Just REPL) = sifREPL

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
    p <- buildPattern (pSpec os) (sSpec os)
    case out os of
      Nothing => printPattern p (to os)
      Just _  => printLn FeatureNotImpl

sifMain : Eff () SifEffs
sifMain = do
    opts <- parseOptions
    putOptions opts
    loadExtLibrary
    runMode (mode opts)

main : IO ()
main = do
  run $ sifMain
  exit 0

-- --------------------------------------------------------------------- [ EOF ]
