-- ------------------------------------------------------------ [ API.idr<Sif> ]
-- Module    : API.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.API

import public Sif.Effs
import public Sif.Error
import public Sif.Pattern
import public Sif.Lib
import public Sif.Parser

import public Sif.Error
import public Sif.Effs

printResults : EvalResult -> Eff () SifEffs
printResults BadModel  = Sif.raise ResultInvalid
printResults (Result xs) = printList xs
  where
    printNode : GoalNode -> Eff () SifEffs
    printNode x = do
      sVal <- fromJustEff $ getSValue x
      putStrLn $ unwords [ show sVal, "==>", getNodeTitle x]

    printList : List GoalNode -> Eff () SifEffs
    printList Nil     = pure ()
    printList (x::xs) = do printNode x; printList xs

doListLibrary : Eff () SifEffs
doListLibrary = do
    lib <- 'lib :- get
    putStrLn "Patterns"
    doList $ getPatternIndex lib
    putStrLn "Problems"
    doList $ getProblemIndex lib
    putStrLn "Solutions"
    doList $ getSolutionIndex lib
  where
    doList : List (Nat, String) -> Eff () SifEffs
    doList Nil = pure ()
    doList ((i,t)::xs) = do
      putStr $ show i
      putStr " <-- "
      putStrLn t
      doList xs

getPattern : Nat -> Eff PATTERN SifEffs
getPattern n = do
  lib <- 'lib :- get
  case index' n (patts lib) of
    Nothing => Sif.raise NoSuchPattern
    Just p' => pure p'

doEvalPattern : Nat -> Eff () SifEffs
doEvalPattern n = do
  p <- getPattern n
  let eRes = evalPattern p
  printResults eRes

doShowPattern : Nat -> Maybe SifOutFormat -> Eff () SifEffs
doShowPattern n Nothing = printLn UnSuppFormat
doShowPattern n (Just fmt) = do
  p <- getPattern n
  putStrLn $ showConvTo p fmt

doChkExtPattern : String -> String -> Eff () SifEffs
doChkExtPattern p s = do
  p <- buildPattern p s
  putStrLn $ unwords ["loaded", getPatternTitle p]
  let eRes = evalPattern p
  printResults eRes


-- --------------------------------------------------------------------- [ EOF ]
