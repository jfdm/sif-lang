-- ------------------------------------------------------------- [ Options.idr ]
-- Module    : Options.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Options

import ArgParse

import Sif.Types

import Effect.Logging.Level

-- ----------------------------------------------------------------- [ Options ]

data SifMode = Eval | Check | Conv | REPL | VERS | HELP

instance Show SifMode where
  show Eval  = "Eval"
  show Check = "Check"
  show Conv  = "Conv"
  show REPL  = "REPL"
  show VERS  = "VERSION"
  show HELP  = "HELP"

instance Eq SifMode where
  (==) Eval  Eval  = True
  (==) Check Check = True
  (==) Conv  Conv  = True
  (==) REPL  REPL  = True
  (==) VERS  VERS  = True
  (==) HELP  HELP  = True
  (==) _     _     = False


record SifOpts where
  constructor MkSOpts
  pSpec      : Maybe String
  sSpec      : Maybe String
  mode       : Maybe SifMode
  out        : Maybe String
  to         : Maybe SifOutFormat
  prelude    : Maybe String
  banner     : Bool
  loglvl     : LogLevel n

defOpts : SifOpts
defOpts = MkSOpts
    Nothing Nothing (Just REPL) Nothing Nothing Nothing
    True OFF

instance Default SifOpts where
  default = defOpts

strToLog : String -> (n ** LogLevel n)
strToLog s =
  case s of
    "1" => (_ ** TRACE)
    "2" => (_ ** DEBUG)
    "3" => (_ ** INFO)
    "4" => (_ ** WARN)
    "5" => (_ ** FATAL)
    "6" => (_ ** ERROR)
    "trace"   => (_ ** TRACE)
    "debug"   => (_ ** DEBUG)
    "info"    => (_ ** INFO)
    "warn"    => (_ ** WARN)
    "fatal"   => (_ ** FATAL)
    "error"   => (_ ** ERROR)
    otherwise => (_ ** OFF)

convOpts : Arg -> SifOpts -> Maybe SifOpts
convOpts (Files xs)     o = Nothing
convOpts (KeyValue k v) o =
  case k of
    "prelude"    => Just $ record {prelude = Just v}                o
    "problem"    => Just $ record {pSpec   = Just v}                o
    "solution"   => Just $ record {sSpec   = Just v}                o
    "out"        => Just $ record {out     = Just v}                o
    "to"         => Just $ record {to      = readOutFMT v}          o
    "log"        => Just $ record {loglvl  = getProof $ strToLog v} o
    otherwise    => Nothing
convOpts (Flag x) o =
  case x of
    "help"     => Just $ record {mode   = Just HELP}  o
    "version"  => Just $ record {mode   = Just VERS}  o
    "check"    => Just $ record {mode   = Just Check} o
    "eval"     => Just $ record {mode   = Just Eval}  o
    "conv"     => Just $ record {mode   = Just Conv}  o
    "nobanner" => Just $ record {banner = False}      o
    "logtrace" => Just $ record {loglvl = TRACE}      o
    "logdebug" => Just $ record {loglvl = DEBUG}      o
    "loginfo"  => Just $ record {loglvl = INFO}       o
    "logwarn"  => Just $ record {loglvl = WARN}       o
    "logfatal" => Just $ record {loglvl = FATAL}      o
    "logerror" => Just $ record {loglvl = ERROR}      o
    otherwise => Nothing

helpStr : String
helpStr = """Sif-Lang (C) Jan de Muijnck-Hughes 2015

Available Options:

Flag                 | Description
---------------------|----------------------------------------------------------
--prelude="<dir>"    | Load an externally defined prelude.
--problem="<fname>"  | A problem specification.
--solution="<fname"  | A solution specification.
--out="<fname>"      | File name to save things to.
--to="<fmt>"         | The output format.
--log="<num|word>"   | Set logging levels
                     | [1,     2,     3,    4,    5,     6]
                     | [trace, debug, info, warn, fatal, error]

--help               | Display help
--version            | Display version
--check              | Check problem solution pairing.
--eval               | Evaluate problem solution pairing
--conv               | Convert problem solution pairing
--nobanner           | Don't display banner

--logtrace
--logdebug
--loginfo
--logwarn
--logfatal
--logerror
"""

-- --------------------------------------------------------------------- [ EOF ]
