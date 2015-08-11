module Sif.Main

import public Sif.Parser
import public Sif.Pattern
import public Sif.API
import public Sif.REPL
import public Sif.Options
import public Sif.Effs

runMode : Maybe SifMode -> Eff () SifEffs
runMode Nothing     = sifREPL
runMode (Just VERS) = printLn FeatureNotImpl
runMode (Just HELP) = printLn FeatureNotImpl

runMode (Just REPL) = sifREPL

runMode (Just Eval) = do
    os <- 'opts :- get
    putStrLn $ "Evaluating Pattern"
    evalPatternFromFile (pSpec os) (sSpec os)

runMode (Just Check) = do
    os <- 'opts :- get
    putStrLn $ "Checking Providing Files"
    doSyntaxCheck (pSpec os) (sSpec os)

runMode (Just Conv) = do
    os <- 'opts :- get
    putStrLn "Converting Pattern"
    p <- buildPattern (pSpec os) (sSpec os)
    case out os of
      Nothing => printPattern p (to os)
      Just _  => printLn FeatureNotImpl

sifMain : List PATTERN -> Eff () SifEffs
sifMain ps = do
    options <- getOptions
    'opts :- put options
    case (prelude options) of
      True => do
        'lib :- update (\lib => addToLibraryM ps lib)
        runMode (mode options)
      False => do
        'lib :- put (addToLibraryM ps emptyLib)
        runMode (mode options)
