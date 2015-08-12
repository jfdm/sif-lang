module Sif.Main

import public Sif.Parser
import public Sif.Pattern
import public Sif.API
import public Sif.REPL
import public Sif.Options
import public Sif.Effs
import public Sif.Library
import public Sif.Prelude

runMode : Maybe SifMode -> Eff () SifEffs
runMode Nothing     = sifREPL
runMode (Just VERS) = printLn FeatureNotImpl
runMode (Just HELP) = printLn FeatureNotImpl

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

sifMain : List PATTERN -> Eff () SifEffs
sifMain ps = do
    opts <- parseOptions
    putOptions opts
    putLibrary (if (prelude opts)
                 then addToLibraryM ps library
                 else addToLibraryM ps emptyLib)
    loadExtLibrary
    runMode (mode opts)
