-- -------------------------------------------------------------- [ Viewer.idr ]
-- Module    : Viewer.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A Pattern Viewer
module Sif.Viewer

import public System

import public Effects
import public Effect.System
import public Effect.State
import public Effect.Exception
import public Effect.File
import public Effect.StdIO

import Lightyear
import Lightyear.Strings

import public Sif.Pattern

import Sif.Viewer.Commands

import Data.GraphViz.SimpleDot

%access public
-- -------------------------------------------------------------------- [ Effs ]

data SifError : Type where
  NoSuchPattern : SifError
  UnSuppFormat  : SifError
  ResultInvalid : SifError
  NoSuchCommand : SifError

instance Show SifError where
  show NoSuchPattern = "No Such Pattern"
  show UnSuppFormat  = "Unsupported output format"
  show ResultInvalid = "Invalid evaluation result"
  show NoSuchCommand = "No such command"

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , EXCEPTION SifError
          , SYSTEM
          , STDIO
          , STATE (List PATTERN)
          ]

{-
fromJustEff : Maybe a -> Eff a [EXCEPTION String]
fromJustEff (Just x) = pure x
fromJustEff Nothing = raise "it was nothing"

doConv : PATTERN -> Eff () SifEffs
doConv p = do
    opts <- 'opts :- get
    case (out opts) of
      Nothing => raise "No output"
      Just x  =>
          case x of
            "dot"     => do
                putStrLn $ show $ API.toDot p
                pure ()
            "xml"     => do
                let xdoc = toXML p
                putStrLn $ show @{xml} xdoc
                pure ()
            -- "org"     => do
            --     case toEdda p of
            --       Nothing => raise "Unable to convert"
            --       Just e  => printLn e
            otherwise => raise "Unsupported out format"


printResults : EvalResult -> Eff () SifEffs
printResults BadModel  = raise "Bad Model"
printResults (Result xs) = printList xs
  where
    printNode : GoalNode -> Eff () SifEffs
    printNode x = putStrLn $ unwords [getNodeTitle x, "==>", show $ getSValue x]

    printList : List GoalNode -> Eff () SifEffs
    printList Nil     = pure ()
    printList (x::xs) = do printNode x; printList xs

doBuildEval : Maybe String
           -> Maybe String
           -> Eff () SifEffs
doBuildEval (Just p) (Just s) = do
    pat <- buildPattern p s
    let res = evalPattern pat
    printResults res
doBuildEval _        _        = raise "Both problem and speficiation need to be given."
-}

fetchCMD : Eff SifCMD SifEffs
fetchCMD = do
    putStr "sif-viewer> "
    rawCmd <- getStr
    case parseCMD rawCmd of
      Left err => do
        putStr "\n"
        printLn NoSuchCommand
        fetchCMD
      Right cmd => pure cmd

||| Fetch and parse commands
private
runViewer : Eff () SifEffs
runViewer = do
    cmd <- fetchCMD
    case cmd of
      (ShowPattern n) => do
          putStrLn "Show Pattern"
          runViewer
      (ListLib)       => do
          putStrLn "List Lib"
          runViewer
      (SavePattern n fmt fname) => do
          putStrLn "Show pattern"
          runViewer
      (EvalPattern n) => do
          putStrLn "Eval Pattern"
          runViewer
      (Quit) => pure ()


||| A Viewer to view a library of patterns.
public
modelViewer : List PATTERN -> Eff () SifEffs
modelViewer ps = do
    put ps
    runViewer

-- --------------------------------------------------------------------- [ EOF ]
