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

buildPatternE : SifBuilder impl
             -> Maybe String
             -> Maybe String
             -> Eff (PATTERN impl) SifEffs
buildPatternE bob Nothing Nothing   = Sif.raise NoFileGiven
buildPatternE bob Nothing  _        = Sif.raise (FileMissing ("Solution File "))
buildPatternE bob _        Nothing  = Sif.raise (FileMissing ("Problem File "))
buildPatternE bob (Just p) (Just s) = do
  let tname = unwords ["Building for ", p, "&", s]
  mkTimer tname
  startTimer tname
  patt <- patternFromFile bob p s
  stopTimer tname
  pure $ patt


evalPatternFromFile : Maybe String -> Maybe String -> Eff () SifEffs
evalPatternFromFile p' s' = do
  bob <- getSifBackend
  p <- buildPatternE (builder bob) p' s'
  putStrLn $ unwords ["loaded", fromMaybe "" $ getTitle p]
  evalAndPrintPattern p


printPattern : PATTERN impl -> Maybe SifOutFormat -> Eff () SifEffs
printPattern _ Nothing        = printLn NoFormatSpecified
printPattern p (Just COMPACT) = putStrLn $ toString p
printPattern p (Just fmt)     = do
    let tname = unwords ["Converting for", show $ getTitle p]
    mkTimer tname
    startTimer tname
    let res = showConvPattern p fmt
    stopTimer tname
    case res of
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
    trace $ unlines [ "Trying to build:"
                    , "\tProblem File: "  ++ show (pDir p)
                    , "\tSolution File: " ++ show (pDir s)]
    bob <- getSifBackend
    patt <- buildPatternE (builder bob) (pDir p) (pDir s)
    updateLibrary (\idx => addToLibrary patt idx)
    importPreludeIDX nspace ps
  where
    pDir : String -> Maybe String
    pDir f = Just $ with List concat [nspace,"/",f]

loadPrelude : Eff () SifEffs
loadPrelude = do
    mkTimer "prelude-loading"
    putLibrary emptyLib
    opts <- getOptions
    case (prelude opts) of
      Nothing   => pure ()
      Just pdir => do
        putStrLn $ unwords ["Importing Prelude:", pdir]
        case !(readYAMLConfig (pdir ++ "/index.yaml")) of
          Left err   => printLn err
          Right pIDX =>
            case filterPreludeIDX pIDX of
              Nil => printLn ImportError
              ps  => do
                  startTimer "prelude-loading"
                  importPreludeIDX pdir ps
                  stopTimer "prelude-loading"

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
