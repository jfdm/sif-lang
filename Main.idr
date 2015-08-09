module Main

import System
import Effects
import Effect.System
import Effect.State
import Effect.Exception
import Effect.File
import Effect.StdIO

import Sif.Pattern
import Sif.Parser

import Data.GraphViz.SimpleDot

import ArgParse

-- ----------------------------------------------------------------- [ Options ]

record SifOpts where
  constructor MkSOpts
  pSpec : Maybe String
  sSpec : Maybe String
  check : Bool
  args  : List String
  help  : Bool
  eval  : Bool
  out   : Maybe String

defOpts : SifOpts
defOpts = MkSOpts Nothing Nothing False Nil False False Nothing

instance Default SifOpts where
  default = defOpts

instance Eq SifOpts where
  (==) (MkSOpts a b c d e f g) (MkSOpts a' b' c' d' e' f' g') =
    a' == a &&
    b' == b &&
    c' == c &&
    d' == d &&
    e' == e &&
    f' == f &&
    g' == g

convOpts : Arg -> SifOpts -> Maybe SifOpts
convOpts (Files xs)     o = Just $ record {args = xs} o
convOpts (KeyValue k v) o =
  case k of
    "problem"  => Just $ record {pSpec = Just v} o
    "solution" => Just $ record {sSpec = Just v} o
    "out"      => Just $ record {out   = Just v} o
    otherwise  => Nothing
convOpts (Flag x) o =
  case x of
    "check"   => Just $ record {check = True} o
    "help"    => Just $ record {help  = True} o
    "eval"    => Just $ record {eval  = True} o
    otherwise => Nothing

parseArgs : List String -> Eff SifOpts [EXCEPTION String]
parseArgs as = do
  res <- parseArgsRec defOpts convOpts as
  pure res

-- -------------------------------------------------------------------- [ Effs ]

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , EXCEPTION String
          , SYSTEM
          , STDIO
          , 'bst  ::: STATE BuildEnv
          , 'opts ::: STATE SifOpts ]

fromJustEff : Maybe a -> Eff a [EXCEPTION String]
fromJustEff (Just x) = pure x
fromJustEff Nothing = raise "it was nothing"

doCheck : Eff () SifEffs
doCheck = do
    opts <- 'opts :- get
    case (sSpec opts, pSpec opts) of
      (Just x, Just y) => do
        readSifFile solution x
        readSifFile problem  y
        pure ()
      (Just x, Nothing) => do
        readSifFile solution x
        pure ()
      (Nothing, Just x) => do
        readSifFile problem x
        pure ()
      (Nothing, Nothing) => raise "Need Files to check."

doConv : SifExpr tyPATTERN -> Eff () SifEffs
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

mainEff : Eff () SifEffs
mainEff = do
  as <- getArgs
  printLn as
  if isNil as
    then raise "No Arguments"
    else do
      os <- parseArgs as
      'opts :- put os
      if (check os)
        then doCheck
        else doBuildEval (pSpec os) (sSpec os)

main : IO ()
main = run mainEff

-- --------------------------------------------------------------------- [ EOF ]
