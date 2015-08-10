-- ------------------------------------------------------------- [ Options.idr ]
-- Module    : Options.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Checker.Options

import public ArgParse

-- ----------------------------------------------------------------- [ Options ]

record SifOpts where
  constructor MkSOpts
  pSpec : Maybe String
  sSpec : Maybe String
  check : Bool
  help  : Bool
  eval  : Bool
  out   : Maybe String

defOpts : SifOpts
defOpts = MkSOpts Nothing Nothing False False False Nothing

instance Default SifOpts where
  default = defOpts

instance Eq SifOpts where
  (==) (MkSOpts a b c d e f) (MkSOpts a' b' c' d' e' f') =
    a' == a &&
    b' == b &&
    c' == c &&
    d' == d &&
    e' == e &&
    f' == f

convOpts : Arg -> SifOpts -> Maybe SifOpts
convOpts (Files xs)     o = Nothing
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

parseArgs : List String -> Eff SifOpts SifEffs
parseArgs as = do
  res <- parseArgsRec defOpts convOpts as
  pure res


-- --------------------------------------------------------------------- [ EOF ]
