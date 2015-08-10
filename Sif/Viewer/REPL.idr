-- -------------------------------------------------------------- [ Viewer.idr ]
-- Module    : Viewer.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A Pattern Viewer
module Sif.Viewer.REPL

import System

import Sif.Viewer.Commands
import Sif.API

%access public
-- -------------------------------------------------------------------- [ Effs ]


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
            lib <- 'lib :- get
            if n < (List.length (patts lib))
              then pure cmd
              else do
               printLn IndexOutOfBounds
               fetchCMD

doCommand : SifCMD -> Eff () SifEffs
doCommand (ShowPattern n fmt fname) = do
  case fname of
    Nothing => do
      putStrLn "Show Pattern"
      doShowPattern n fmt
    Just fn => do
      putStrLn $ "Saving Pattern to " ++ fn ++ " as " ++ show fmt
      putStrLn "TODO"

doCommand (ListLib) = do
  putStrLn "Listing Library"
  doListLibrary

doCommand (EvalPattern n) = do
  putStrLn "Eval Pattern"
  doEvalPattern n

doCommand (CheckExtPattern p s) = do
  putStrLn "Importing..."
  doChkExtPattern p s

doCommand (Quit) = pure ()

runViewer : Eff () SifEffs
runViewer = do
    cmd <- fetchCMD
    case cmd of
      Quit => pure ()
      x    => do
        doCommand x
        runViewer


||| A Viewer to view a library of patterns.
public
modelViewer : SifLib -> Eff () SifEffs
modelViewer extst = do
    'lib :- update (\st => cmbState st extst)
    runViewer

-- --------------------------------------------------------------------- [ EOF ]
