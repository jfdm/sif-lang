-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.API

import Effects
import Effect.System
import Effect.Default
import Effect.State
import Effect.Exception
import Effect.File
import Effect.StdIO
import Effect.Logging.Default
import Effect.Perf

import Lightyear
import Lightyear.Strings

import Config.Error
import Config.YAML

import Data.GraphViz.SimpleDot
import XML.DOM

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern
import Sif.DSL.State
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution
import Sif.DSL.Parser
import Sif.DSL

import Sif.Effs
import Sif.Error
import Sif.Library

import Sif.Options
import Sif.Prelude

%default partial
%access public


||| Print results of evaluation
printResults : EvalResult -> Sif ()
printResults Bad        = Sif.raise ResultInvalid
printResults (Good is)  = do
    let is' = map (\(t,s) => unwords [showSValue s,"\t==>\t", t]) is
    putStrLn $ unlines is'
  where
    showSValue : Maybe SValue -> String
    showSValue Nothing  = "Nothing"
    showSValue (Just x) = show x

||| Syntax check a Problem or a solution pairing.
doSyntaxCheck : Maybe String -> Maybe String -> Sif ()

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


evalAndPrintPattern : (PATTERN impl d) -> Sif ()
evalAndPrintPattern p = do
  let tname = unwords ["Evaluating:", SifExpr.getTitle p]
  trace tname
  mkTimer tname
  startTimer tname
  let eRes = evalPattern p
  stopTimer tname
  printResults eRes

tryToBuildPatternE : SifBuilder impl
             -> Maybe String
             -> Maybe String
             -> Sif (d ** PATTERN impl d)
tryToBuildPatternE bob Nothing Nothing   = Sif.raise NoFileGiven
tryToBuildPatternE bob Nothing  _        = Sif.raise (FileMissing ("Solution File "))
tryToBuildPatternE bob _        Nothing  = Sif.raise (FileMissing ("Problem File "))
tryToBuildPatternE bob (Just p) (Just s) = do
  let tname = unwords ["Building for ", p, "&", s]
  mkTimer tname
  startTimer tname
  patt <- patternFromFile bob p s
  stopTimer tname
  pure patt


evalPatternFromFile : Maybe String -> Maybe String -> Sif ()
evalPatternFromFile p' s' = do
  bob <- getSifBackend
  (_ ** p) <- tryToBuildPatternE (builder bob) p' s'
  putStrLn $ unwords ["loaded", SifExpr.getTitle p]
  evalAndPrintPattern p

printPattern : PATTERN impl d -> Maybe SifOutFormat -> Sif ()
printPattern _ Nothing    = printLn NoFormatSpecified
printPattern p (Just fmt) = do
    let tname = unwords ["Converting", show $ SifExpr.getTitle p, "to", show fmt]
    mkTimer tname
    startTimer tname
    let res = showConvPattern fmt p
    stopTimer tname
    case res of
      Nothing  => printLn UnSuppFormat
      Just res => putStrLn res

convPatternFromFile : Maybe String
                   -> Maybe String
                   -> Maybe String
                   -> Maybe SifOutFormat
                   -> Sif ()
convPatternFromFile p s fname fmt = do
    bob <- getSifBackend
    (_ ** p) <- tryToBuildPatternE (builder bob) p s
    case fname of
      Nothing => printPattern p fmt
      Just _  => printLn FeatureNotImpl

-- ------------------------------------------------------- [ Library Functions ]

importPreludeIDX : String -> List (String, String) -> Sif ()
importPreludeIDX _      Nil         = pure ()
importPreludeIDX nspace ((p,s)::ps) = do
    trace $ unlines [ "Trying to build:"
                    , "\tProblem File: "  ++ show (pDir p)
                    , "\tSolution File: " ++ show (pDir s)]
    bob <- getSifBackend
    (_ ** patt) <- tryToBuildPatternE (builder bob) (pDir p) (pDir s)
    updateLibrary (\idx => addToLibrary patt idx)
    importPreludeIDX nspace ps
  where
    pDir : String -> Maybe String
    pDir f = Just $ with List concat [nspace,"/",f]

loadPrelude : Sif ()
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

listLibrary : Sif ()
listLibrary = do
    l <- getLibrary
    let idx = (getLibraryIndex l)
    putStrLn "Patterns"
    putStrLn (indexToString idx)


getPatternByIndexEff : Nat -> Sif (d ** (impl ** PATTERN impl d))
getPatternByIndexEff n =
  case getPatternByIndex n !getLibrary of
    Nothing => Sif.raise NoSuchPattern
    Just p' => pure p'

getAndEvalPattern : Nat -> Sif ()
getAndEvalPattern n = do
  (_ ** (_ ** p)) <- getPatternByIndexEff n
  evalAndPrintPattern p

getAndPrintPattern : Nat
                  -> Maybe SifOutFormat
                  -> Sif ()
getAndPrintPattern n fmt = do
  (_ ** (_ ** p)) <- getPatternByIndexEff n
  printPattern p fmt

-- --------------------------------------------------------------------- [ EOF ]
