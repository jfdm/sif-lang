-- ---------------------------------------------------------------- [ Perf.idr ]
-- Module    : Perf.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A naive performance effect for gathering data about the
||| performance of effectful programmes.
module Effect.Perf

import System

import Effects

-- --------------------------------------------------------- [ Data Structures ]

record Timer where
  constructor MkTimer
  desc   : String
  start  : Integer
  stop   : Integer
  splits : List (Integer, Maybe String)

instance Show Timer where
  show (MkTimer d a z bcd) = unlines
      [ "Timer: " ++ d
      , "\tStart:\t" ++ show a ++ (showSplits bcd)
      , "\tStop:\t"  ++ show z
      , "\tDifference:\t" ++ show (z - a)
      ]
    where
      showSplit : (Integer, Maybe String) -> String
      showSplit (v, Nothing)  = with List unwords ["\tSplit:\t",show v]
      showSplit (v, Just msg) = with List unwords ["\tSplit:\t",show v, show msg]

      doShow : List (Integer, Maybe String) -> List String
      doShow ss = map showSplit $ reverse ss

      showSplits : List (Integer, Maybe String) -> String
      showSplits Nil = ""
      showSplits ss  = unlines $ ("" :: doShow ss)

defTimer : String -> Timer
defTimer n = MkTimer n 0 0 Nil

-- ---------------------------------------------------------------- [ Resource ]
record PMetrics where
  constructor MkPMetrics
  canPerf  : Bool
  livePerf : Bool
  counters : List (String, Nat)
  timers   : List (String, Timer)
  stime    : Integer
  stamps   : List (String, Integer)


instance Default PMetrics where
  default = MkPMetrics False False Nil Nil 0 Nil

instance Show PMetrics where
  show (MkPMetrics c l cs ts _ ss) =
    unlines [ "Perf Data:"
            , unlines $ map showCounter cs
            , unlines $ map (\(x,y) => show y) ts
            , unlines $ map showStamp ss]
    where
      showCounter : Pair String Nat -> String
      showCounter (d,v) = unwords [show v,"\t<==\t", d]

      showStamp : Pair String Integer -> String
      showStamp (d,v) = unwords [show v,"\t<==\t", d]


incCounter' : String -> PMetrics -> PMetrics
incCounter' x st = record {counters = cs} st
  where
    cs : List (String, Nat)
    cs = map (\(y,c) => if x == y then (y,S c) else (y,c)) (counters st)


data TOpt = START | STOP | SPLIT

instance Show TOpt where
  show START = "Starting"
  show STOP  = "Stopping"
  show SPLIT = "Splitting"

timerStuff' : TOpt -> Integer -> String -> Maybe String -> PMetrics -> PMetrics
timerStuff' opt val x msg st = record {timers = ts} st
  where
    doThing : (String,Timer) -> (String,Timer)
    doThing (y,t) =
      if not (x == y)
        then (y, t)
        else case opt of
          START => MkPair y $ record {start  = val} t
          STOP  => MkPair y $ record {stop   = val} t
          SPLIT => MkPair y $ record {splits = (val, msg) :: splits t} t

    ts : List (String, Timer)
    ts = map doThing (timers st)


perfLog : PMetrics -> String -> IO ()
perfLog res msg =
    if livePerf res
      then putStrLn $ unwords ["PERF:", msg]
      else return ()

-- ------------------------------------------------------- [ Effect Definition ]

data Perf : Effect where
  GetMetrics     : sig Perf (PMetrics) (PMetrics)
  TurnOn      : Bool -> sig Perf () (PMetrics) (PMetrics)
  MkCounter   : String -> sig Perf () (PMetrics) (PMetrics)
  IncCounter  : String -> sig Perf () (PMetrics) (PMetrics)
  Timestamp   : String -> sig Perf () (PMetrics) (PMetrics)
  MkStopWatch : String -> sig Perf () (PMetrics) (PMetrics)
  TimerStuff  : TOpt -> String -> Maybe String -> sig Perf () (PMetrics) (PMetrics)

-- ---------------------------------------------------------- [ Handler for IO ]

instance Handler Perf IO where
  handle res (TurnOn b) k = do
      v <- time
      let res' = record {canPerf = True, livePerf = b, stime = v} res
      perfLog res' $ unwords [ "Turning on Perf with at", show v]
      k () (res')

  handle res (GetMetrics)  k = k res res

  handle res (MkCounter n) k = do
      if not (canPerf res)
        then k () res
        else do
          let res' = record {counters = (n,Z) :: counters res} res
          perfLog res' $ unwords ["Creating counter:", show n]
          k () (res')

  handle res (IncCounter n) k = do
      if not (canPerf res)
        then k () res
        else do
          let res' = incCounter' n res
          perfLog res' $ unwords ["Incriminating counter:", show n]
          k () (res')

  handle res (MkStopWatch n) k = do
      if not (canPerf res)
        then k () res
        else do
          let res' = record {timers = (n, defTimer n) :: timers res} res
          perfLog res' $ unwords ["Creating Timer:", show n]
          k () (res')

  handle res (TimerStuff opt n msg) k = do
      v <- time
      if not (canPerf res)
        then k () res
        else do
          let res' = timerStuff' opt v n msg res
          perfLog res' $ unwords [show opt, "timer", show n, "at", show v, fromMaybe "" msg]
          k () (res')

  handle res (Timestamp s) k = do
      v <- time
      let stamp = v - (stime res)
      if not (canPerf res)
        then k () res
        else do
          let res' = record {stamps = (s,stamp) :: stamps res} res
          perfLog res' $ unlines [ "Creating Timestamp:"
                                 , "\tTIMESTAMP:", show v
                                 , "\tMESSAGE:", show s]
          k () (res')

-- ------------------------------------------------------- [ Effect Descriptor ]

PERF : EFFECT
PERF = MkEff PMetrics Perf

-- --------------------------------------------------------------------- [ API ]

||| Turn on performance metrics.
collectPMetricsOnly : Eff () [PERF]
collectPMetricsOnly = call $ TurnOn False

collectPMetrics : Bool -> Eff () [PERF]
collectPMetrics b = call $ TurnOn b

||| Turn on performance metrics and show during operation
collectPMetricsAndShow : Eff () [PERF]
collectPMetricsAndShow = call $ TurnOn True

||| Return gatheres metrics
getPerfMetrics : Eff PMetrics [PERF]
getPerfMetrics = call $ GetMetrics

||| Create a counter
mkCounter : String -> Eff () [PERF]
mkCounter n = call $ MkCounter n

||| Increment the counter
incCounter : String -> Eff () [PERF]
incCounter n = call $ IncCounter n

mkTimer : String -> Eff () [PERF]
mkTimer n = call $ MkStopWatch n

||| Stop a timer
stopTimer : String -> Eff () [PERF]
stopTimer n = call $ TimerStuff STOP n Nothing

||| Start a timer
startTimer : String -> Eff () [PERF]
startTimer n = call $ TimerStuff START n Nothing

||| Split a timer
splitTimer : String -> Eff () [PERF]
splitTimer n = call $ TimerStuff SPLIT n Nothing

splitTimerMsg : String -> String -> Eff () [PERF]
splitTimerMsg n msg = call $ TimerStuff SPLIT n (Just msg)

||| Create a time stamp.
timestamp : String -> Eff () [PERF]
timestamp msg = call $ Timestamp msg

-- --------------------------------------------------------------------- [ EOF ]
