-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.API

import public Config.Error
import public Config.YAML

import Data.GraphViz.SimpleDot
import XML.DOM

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

%default partial

%access public


||| Print results of evaluation
printResults : EvalResult -> Eff () SifEffs
printResults Bad        = Sif.raise ResultInvalid
printResults (Good is)  = do
    let is' = map (\(t,s) => unwords [showSValue s,"\t==>\t", t]) is
    putStrLn $ unlines is'
  where
    showSValue : Maybe SValue -> String
    showSValue Nothing  = "Nothing"
    showSValue (Just x) = show x

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


evalAndPrintPattern : (PATTERN impl) -> Eff () SifEffs
evalAndPrintPattern p = do
  putStrLn $ unwords ["evaluating pattern", fromMaybe "" $ getTitle p]
  let eRes = evalPattern p
  printResults eRes

buildPatternE : Maybe String -> Maybe String -> Eff (impl ** PATTERN impl) SifEffs
buildPatternE Nothing Nothing   = Sif.raise NoFileGiven
buildPatternE Nothing  _        = Sif.raise (FileMissing ("Solution File "))
buildPatternE _        Nothing  = Sif.raise (FileMissing ("Problem File "))
buildPatternE (Just p) (Just s) = do
  (_ ** bob) <- getSifBuilder
  patt <- patternFromFile bob p s
  pure $ (_ ** patt)


evalPatternFromFile : Maybe String -> Maybe String -> Eff () SifEffs
evalPatternFromFile p' s' = do
  (_ ** p) <- buildPatternE p' s'
  putStrLn $ unwords ["loaded", fromMaybe "" $ getTitle p]
  evalAndPrintPattern p


printPattern : PATTERN impl -> Maybe SifOutFormat -> Eff () SifEffs
printPattern _ Nothing        = printLn NoFormatSpecified
printPattern p (Just COMPACT) = putStrLn $ toString p
printPattern p (Just fmt)     = do
    case showConvPattern p fmt of
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
    putStrLn $ unwords ["Importing Prelude:", nspace]
    info $ unlines [ "Trying to build:"
                   , "\tProblem File: "  ++ show (pDir p)
                   , "\tSolution File: " ++ show (pDir s)]
    (_ ** bob)  <- getSifBuilder
    patt <- patternFromFile bob (pDir p) (pDir s)
    updateLibrary (\idx => addToLibrary patt idx)
    importPreludeIDX nspace ps
  where
    pDir : String -> String
    pDir f = nspace ++ "/" ++ f

loadPrelude : Eff () SifEffs
loadPrelude = do
    putLibrary emptyLib
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
    l <- getLibrary
    let idx = (getLibraryIndex l)
    putStrLn "Patterns"
    putStrLn (indexToString idx)


getPatternByIndexEff : Nat -> Eff (impl ** PATTERN impl) SifEffs
getPatternByIndexEff n =
  case getPatternByIndex n !getLibrary of
    Nothing => Sif.raise NoSuchPattern
    Just p' => pure p'

getAndEvalPattern : Nat -> Eff () SifEffs
getAndEvalPattern n = do
  (_ ** p) <- getPatternByIndexEff n
  evalAndPrintPattern p

getAndPrintPattern : Nat -> Maybe SifOutFormat -> Eff () SifEffs
getAndPrintPattern n fmt = do
  (_ ** p) <- getPatternByIndexEff n
  printPattern p fmt

-- --------------------------------------------------------------------- [ EOF ]
