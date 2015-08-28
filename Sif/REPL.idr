-- -------------------------------------------------------------- [ Viewer.idr ]
-- Module    : Viewer.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A Pattern Viewer
module Sif.REPL

import System

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern
import Sif.Effs
import Sif.Error
import Sif.Library
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution
import Sif.DSL.Parser
import Sif.DSL
import Sif.Options
import Sif.API

import Sif.Commands

%default partial
%access public
-- -------------------------------------------------------------------- [ Effs ]

sifBanner : String
sifBanner = """
   _____ _ ____   __
  / ___/(_) __/  / /   ____ _____  ____ _
  \__ \/ / /_   / /   / __ `/ __ \/ __ `/
 ___/ / / __/  / /___/ /_/ / / / / /_/ /
/____/_/_/    /_____/\__,_/_/ /_/\__, /
                                /____/

http://www.github.com/jfdm/sif-lang
Type :? for help

Sif is free software with ABSOLUTELY NO WARRANTY.
"""

||| Fetch and parse commands
fetchCMD : Eff SifCMD SifEffs
fetchCMD = do
    putStr "sif-viewer> "
    rawCmd <- getStr
    case parseCMD rawCmd of
      Left err => do
        printLn NoSuchCommand
        fetchCMD
      Right cmd => do
        case getCmdIndex cmd of
          Nothing => pure cmd
          Just n  => do
            lib <- getLibrary
            if n < (length $ patts lib)
              then pure cmd
              else do
               printLn IndexOutOfBounds
               fetchCMD

covering
doCommand : SifCMD -> Eff () SifEffs
doCommand Quit = pure ()
doCommand Help = putStrLn showHelp

doCommand (ShowPattern n fmt fname) = do
  case fname of
    Nothing => do
      putStrLn "Show Pattern"
      getAndPrintPattern n fmt
    Just fn => do
      putStrLn $ "Saving Pattern to " ++ fn ++ " as " ++ show fmt
      printLn FeatureNotImpl

doCommand (ListLib) = do
  putStrLn "Listing Library"
  listLibrary

doCommand (PreludeLoad x) = do
  case x of
    Nothing => loadPrelude
    dirname => do
      updateOptions (\o => record {prelude = dirname} o)
      loadPrelude

doCommand (EvalPattern n) = do
  putStrLn "Eval Pattern"
  getAndEvalPattern n

doCommand (CheckExtPattern p s) = do
  putStrLn "Importing..."
  evalPatternFromFile (Just p) (Just s)
doCommand _ = printLn NoSuchCommand


runREPL : Eff () SifEffs
runREPL = do
    cmd <- fetchCMD
    case cmd of
      Quit => pure ()
      x    => do
        doCommand x
        runREPL


||| A Viewer to view a library of patterns.
public
sifREPL : Eff () SifEffs
sifREPL =
  case banner !(getOptions) of
    True => do
      putStrLn sifBanner
      runREPL
    False => runREPL

-- --------------------------------------------------------------------- [ EOF ]
