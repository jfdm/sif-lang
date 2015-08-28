-- --------------------------------------------------------------- [ Types.idr ]
-- Module    : Types.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Types

data RTy = FUNC | USAB | RELI | PERF | SUPP

instance Cast RTy String where
  cast FUNC = "functional"
  cast USAB = "usability"
  cast RELI = "reliability"
  cast PERF = "performance"
  cast SUPP = "supportability"

instance Show RTy where
  show FUNC = "Functional"
  show USAB = "Usability"
  show RELI = "Reliability"
  show PERF = "Performance"
  show SUPP = "Supportability"

data TTy = ADV  | DIS | GEN

instance Show TTy where
  show ADV = "Advantage"
  show DIS = "Disadvantage"
  show GEN = "Trait"

data STy = ABSTRACT | CONCRETE

data SifTy = tyREQ     | tyTRAIT   | tyPROPERTY | tySOLUTION
           | tyPROBLEM | tyPATTERN | tyAFFECTS

data SifOutFormat = ORG | XML | DOT | GRL | EDDA | COMPACT | IDRIS

instance Eq SifOutFormat where
  (==) ORG     ORG     = True
  (==) XML     XML     = True
  (==) DOT     DOT     = True
  (==) GRL     GRL     = True
  (==) EDDA    EDDA    = True
  (==) COMPACT COMPACT = True
  (==) IDRIS   IDRIS   = True
  (==) _       _       = False

instance Show SifOutFormat where
  show ORG = "Org"
  show XML = "XML"
  show DOT = "dot"
  show GRL = "GRL"
  show EDDA = "Edda"
  show COMPACT = "Compact"
  show IDRIS   = "Idris"

readOutFMT : String -> Maybe SifOutFormat
readOutFMT s =
  case s of
    "org" => Just ORG
    "xml" => Just XML
    "dot" => Just DOT
    "grl" => Just GRL
    "compact" => Just COMPACT
    "idris"   => Nothing
    "edda" => Nothing -- TODO Just EDDA
    otherwise => Nothing

-- --------------------------------------------------------------------- [ EOF ]
