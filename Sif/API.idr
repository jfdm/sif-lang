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

||| COnv and SHow -- horrible horrible code.
showConvTo : PATTERN -> (fmt : SifOutFormat) -> Maybe String
showConvTo p GRL = Just $ show (the (convTy GRL) (convTo p GRL))
showConvTo p DOT = Just $ show (the (convTy DOT) (convTo p DOT))
showConvTo p XML = Just $ show @{xml} $ (the (convTy XML) (convTo p XML))
showConvTo p ORG = Just $ (the (convTy ORG) (convTo p ORG))
showConvTo p EDDA = Nothing


||| Print results of evaluation
printResults : EvalResult -> Eff () SifEffs
printResults BadModel = Sif.raise ResultInvalid
printResults res      =
    case EvalResult.toString res printNode of
      Nothing    => Sif.raise ResultInvalid
      (Just str) => putStrLn str
  where
    printNode : GoalNode -> String
    printNode x = unwords [ show $ getSValue x, "==>", getNodeTitle x]

||| Syntax check a Problem or a solution pairing.
doSyntaxCheck : Maybe String -> Maybe String -> Eff () SifEffs

doSyntaxCheck (Just x) (Just y) = do
    readSifFile problem x
    readSifFile solution y
    pure ()

doSyntaxCheck (Just x) (Nothing) = do
    readSifFile problem x
    pure ()

doSyntaxCheck (Nothing) (Just y) = do
    readSifFile solution y
    pure ()

doSyntaxCheck (Nothing) (Nothing) = Sif.raise NoFileGiven


evalAndPrintPattern : PATTERN -> Eff () SifEffs
evalAndPrintPattern p = do
  putStrLn $ unwords ["evaluating pattern", getPatternTitle p]
  let eRes = evalPattern p
  printResults eRes

buildPattern : Maybe String -> Maybe String -> Eff PATTERN SifEffs
buildPattern Nothing Nothing   = Sif.raise NoFileGiven
buildPattern Nothing  _        = Sif.raise (FileMissing ("Solution File "))
buildPattern _        Nothing  = Sif.raise (FileMissing ("Problem File "))
buildPattern (Just p) (Just s) = buildPatternFromFile p s


evalPatternFromFile : Maybe String -> Maybe String -> Eff () SifEffs
evalPatternFromFile p' s' = do
  p <- buildPattern p' s'
  putStrLn $ unwords ["loaded", getPatternTitle p]
  evalAndPrintPattern p


printPattern : PATTERN -> Maybe SifOutFormat -> Eff () SifEffs
printPattern _ Nothing    = printLn NoFormatSpecified
printPattern p (Just fmt) = do
    case showConvTo p fmt of
      Nothing  => printLn UnSuppFormat
      Just res => putStrLn res

-- ------------------------------------------------------- [ Library Functions ]

listLibrary : Eff () SifEffs
listLibrary = do
    lib <- 'lib :- get
    let idx = (getLibraryIndex lib)
    putStrLn "Patterns"
    putStrLn (indexToString idx)


getPatternByIndexEff : Nat -> Eff PATTERN SifEffs
getPatternByIndexEff n =
  case getPatternByIndex n !('lib :- get) of
    Nothing => Sif.raise NoSuchPattern
    Just p' => pure p'

getAndEvalPattern : Nat -> Eff () SifEffs
getAndEvalPattern n = do
  p <- getPatternByIndexEff n
  evalAndPrintPattern p

getAndPrintPattern : Nat -> Maybe SifOutFormat -> Eff () SifEffs
getAndPrintPattern n fmt = do
  p <- getPatternByIndexEff n
  printPattern p fmt


-- --------------------------------------------------------------------- [ EOF ]
