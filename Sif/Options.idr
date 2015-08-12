-- ------------------------------------------------------------- [ Options.idr ]
-- Module    : Options.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Options

import ArgParse
import Sif.Pattern

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
  extprelude : Maybe String
  prelude    : Bool
  banner     : Bool


defOpts : SifOpts
defOpts = MkSOpts
    Nothing Nothing (Just REPL) Nothing Nothing Nothing
    False True

instance Default SifOpts where
  default = defOpts

instance Eq SifOpts where
  (==) (MkSOpts a b c d e f g h) (MkSOpts a' b' c' d' e' f' g' h') =
    a' == a && b' == b && c' == c && d' == d &&
    e' == e && f' == f && g' == g && h' == h

convOpts : Arg -> SifOpts -> Maybe SifOpts
convOpts (Files xs)     o = Nothing
convOpts (KeyValue k v) o =
  case k of
    "extprelude" => Just $ record {extprelude = Just v}       o
    "problem"    => Just $ record {pSpec      = Just v}       o
    "solution"   => Just $ record {sSpec      = Just v}       o
    "out"        => Just $ record {out        = Just v}       o
    "to"         => Just $ record {to         = readOutFMT v} o
    otherwise    => Nothing
convOpts (Flag x) o =
  case x of
    "help"     => Just $ record {mode    = Just HELP}  o
    "version"  => Just $ record {mode    = Just VERS}  o
    "check"    => Just $ record {mode    = Just Check} o
    "eval"     => Just $ record {mode    = Just Eval}  o
    "conv"     => Just $ record {mode    = Just Conv}  o
    "nobanner" => Just $ record {banner  = False}      o
    "prelude"  => Just $ record {prelude = True}       o
    otherwise => Nothing

-- --------------------------------------------------------------------- [ EOF ]
