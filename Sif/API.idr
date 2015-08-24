-- ------------------------------------------------------------ [ API.idr<Sif> ]
-- Module    : API.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.API

import public Config.Error
import public Config.YAML

import Data.GraphViz.SimpleDot
import XML.DOM

import Sif.Effs
import Sif.Error
import Sif.Pattern
import Sif.Library
import Sif.Parser
import Sif.Options

%default partial

%access public


||| COnv and SHow -- horrible horrible code.
partial covering
showConvTo : PATTERN -> (fmt : SifOutFormat) -> Maybe String
showConvTo p GRL = Just $ show (the (convTy GRL) (convTo p GRL))
showConvTo p DOT = Just $ show (the (convTy DOT) (convTo p DOT))
showConvTo p XML = Just $ show @{xml} $ (the (convTy XML) (convTo p XML))
showConvTo p ORG = Just $ (the (convTy ORG) (convTo p ORG))
showConvTo p COMPACT = Just $ (the (convTy COMPACT) (convTo p COMPACT))
showConvTo p IDRIS   = Nothing
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
printPattern _ Nothing        = printLn NoFormatSpecified
printPattern p (Just COMPACT) = printLn p
printPattern p (Just fmt) = do
    case showConvTo p fmt of
      Nothing      => printLn UnSuppFormat
      Just res     => putStrLn res

-- ------------------------------------------------------------ [ YAML Parsing ]

{-
Schema is

file ::= pattern+
pattern ::= "pattern": {"problem":!!str, "solution": (vale::!!str)}

-}

private
getString : YAMLNode -> Maybe String
getString (YAMLString s) = Just s
getString _              = Nothing

private
isPatternMap : YAMLNode -> Bool
isPatternMap (YAMLString str) = toLower str == "pattern"
isPatternMap _ = False

private
getPatternKVPairs : YAMLNode -> List (YAMLNode)
getPatternKVPairs (YAMLDoc _ doc) with (doc)
  | (YAMLMap ps) = map snd $ filter (\(x,y) => isPatternMap x) ps
  | otherwise = Nil
getPatternKVPairs _ = Nil

private
getStringKey : (YAMLNode, YAMLNode) -> Maybe String
getStringKey (k,_) = getString k

private
getStringValue : (YAMLNode, YAMLNode) -> Maybe String
getStringValue (_,v) = getString v

private
getPSPair : YAMLNode -> Maybe (String, String)
getPSPair (YAMLMap [p,s]) =
    case (getStringKey p, getStringKey s) of
      (Just p', Just s') =>
        if toLower p' == "problem" && toLower s' == "solution"
          then case (getStringValue p, getStringValue s) of
            (Just p'', Just s'') => Just (p'', s'')
            otherwise            => Nothing
          else Nothing
      otherwise          => Nothing
getPSPair _  = Nothing

private
filterPreludeIDX : YAMLNode -> List (String, String)
filterPreludeIDX doc = mapMaybe (getPSPair) $ getPatternKVPairs doc


-- ------------------------------------------------------- [ Library Functions ]

importPreludeIDX : String -> List (String, String) -> Eff () SifEffs
importPreludeIDX _      Nil         = pure ()
importPreludeIDX nspace ((p,s)::ps) = do
    putStrLn $ unlines [ "Trying to build:"
                       , "\tProblem File: " ++ show (pDir p)
                       , "\tSolution File: " ++ show (pDir s)]
    patt <- buildPatternFromFile (pDir p) (pDir s)
    updateLibrary (\idx => addToLibrary patt idx)
    importPreludeIDX nspace ps
  where
    pDir : String -> String
    pDir f = nspace ++ "/" ++ f

loadExtLibrary : Eff () SifEffs
loadExtLibrary = do
    opts <- getOptions
    case (prelude opts) of
      Nothing   => pure ()
      Just pdir => do
        case !(readYAMLConfig (pdir ++ "/index.yaml")) of
          Left err   => printLn err
          Right pIDX =>
            case filterPreludeIDX pIDX of
              Nil => printLn ImportError
              ps  => importPreludeIDX pdir ps

listLibrary : Eff () SifEffs
listLibrary = do
    lib <- getLibrary
    let idx = (getLibraryIndex lib)
    putStrLn "Patterns"
    putStrLn (indexToString idx)


getPatternByIndexEff : Nat -> Eff PATTERN SifEffs
getPatternByIndexEff n =
  case getPatternByIndex n !getLibrary of
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
