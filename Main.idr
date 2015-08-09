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

fromJustEff : Maybe a -> Eff a [EXCEPTION String]
fromJustEff (Just x) = pure x
fromJustEff Nothing = raise "it was nothing"

doCheck : SifOpts
       -> Eff () [FILE_IO (), EXCEPTION String, STATE SifState, SYSTEM, STDIO]
doCheck opts =
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

doConv : SifOpts
      -> SifExpr tyPATTERN
      -> Eff () [FILE_IO (), EXCEPTION String, STATE SifState, SYSTEM, STDIO]
doConv opts p =
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


mainEff : Eff () [FILE_IO (), EXCEPTION String, STATE SifState, SYSTEM, STDIO]
mainEff = do
  as <- getArgs
  printLn as
  if isNil as
    then raise "No Arguments"
    else do
      opts <- parseArgs as
      if check opts
        then doCheck opts
        else do
          if isJust (sSpec opts) && isJust (pSpec opts)
            then do
              pro <- fromJustEff (pSpec opts)
              sol <- fromJustEff (sSpec opts)
              p <- buildPattern pro sol
              if (eval opts)
                then do
                  let m = getModel p
                  let res = evalModel m Nothing
                  printLn res
                else doConv opts p
            else raise "Both problem and speficiation need to be given."

main : IO ()
main = run mainEff

-- --------------------------------------------------------------------- [ EOF ]
