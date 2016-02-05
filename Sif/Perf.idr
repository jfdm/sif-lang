-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Perf

import Effect.Perf

import Config.YAML

import Sif.Effs
import Sif.Options
import Sif.FileIO

%access export

private
convMaybe : Maybe String -> YAMLNode
convMaybe Nothing  = YAMLNull
convMaybe (Just m) = YAMLString m

private
convSplit : Pair Integer (Maybe String)
         -> Pair YAMLNode YAMLNode
convSplit (t,d) = MkPair
    (YAMLString "split")
    (YAMLMap
      [ MkPair (YAMLString "time") (YAMLFloat $ cast t)
      , MkPair (YAMLString "desc") (convMaybe d)])

private
doCounters : List (String, Nat) -> Pair YAMLNode YAMLNode
doCounters cs = MkPair (YAMLString "counters")
                       (YAMLMap $ map doConv cs)
  where
    doConv : Pair String Nat
          -> Pair YAMLNode YAMLNode
    doConv (k,v) = MkPair (YAMLString "counter")
                          (YAMLMap
      [ MkPair (YAMLString "value") (YAMLInt $ cast k)
      , MkPair (YAMLString "desc")  (YAMLString k)
      ])

private
doTimers : List (String, Timer) -> Pair YAMLNode YAMLNode
doTimers ts = MkPair (YAMLString "timers")
                     (YAMLMap (map doConv ts))
  where
    convSplits : List (Integer, Maybe String)
              -> List (YAMLNode, YAMLNode)
    convSplits xs = map convSplit xs

    convTimer : Timer -> YAMLNode
    convTimer (MkTimer d a z ss) = YAMLMap
        [ MkPair (YAMLString "desc")   (YAMLString d)
        , MkPair (YAMLString "start")  (YAMLFloat $ cast a)
        , MkPair (YAMLString "start")  (YAMLFloat $ cast z)
        , MkPair (YAMLString "splits") (YAMLMap $ convSplits ss)]

    doConv : Pair String Timer -> Pair YAMLNode YAMLNode
    doConv (k,v) = MkPair (YAMLString "timer")
                          (convTimer v)
private
doStamps : Integer
        -> List (String, Integer)
        -> Pair YAMLNode YAMLNode
doStamps s ss = MkPair (YAMLString "timestamps")
                       (YAMLMap $ map doConv ss)
  where
    doConv : Pair String Integer -> Pair YAMLNode YAMLNode
    doConv (m,i) = MkPair (YAMLString "stamp")
                          (YAMLMap [convSplit (i - s, Just m)])
private
toYAML : PMetrics -> YAMLNode
toYAML m = YAMLDoc Nil inner
  where
    inner : YAMLNode
    inner = YAMLMap
        [ doCounters (counters m)
        , doTimers (timers m)
        , doStamps (stime m) (stamps m)]


perfSetup : Sif ()
perfSetup = do
    os <- getOptions
    let (gather,display) = perf os
    if gather
      then collectPMetrics display
      else pure ()

displayPerfMetrics : Sif ()
displayPerfMetrics = do
    os <- getOptions
    mdata <- getPerfMetrics
    case perf os of
      (True, True)  => do
          printLn $ displayPerfMetrics mdata
          writeFile "perf.yaml" (YAML.toString $ toYAML mdata)
      (True, False) => do
          writeFile "perf.yaml" (YAML.toString $ toYAML mdata)
      otherwise => pure ()

-- --------------------------------------------------------------------- [ EOF ]
