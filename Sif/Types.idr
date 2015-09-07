-- --------------------------------------------------------------- [ Types.idr ]
-- Module    : Types.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Types

-- ----------------------------------------------- [ Problems and Requirements ]

-- Encoding of CONTROL | CODE | ACTION ??
-- Encoding of Categories => Security, Access Control, HCI...

-- These internal types are not used. How to use them?

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


data HasMData : SifTy -> Type where
  HMA : HasMData tyREQ
  HMB : HasMData tyPROPERTY
  HMC : HasMData tySOLUTION
  HMD : HasMData tyPROBLEM
  HME : HasMData tyPATTERN
  HMF : HasMData tyTRAIT
  HMG : HasMData tyDOMAIN


data SifOutFormat = ORG  | LATEX   | CMARK | XML    | DOT
                  | EDDA | COMPACT | IDRIS | STRING

instance Eq SifOutFormat where
  (==) LATEX   LATEX   = True
  (==) CMARK   CMARK   = True
  (==) ORG     ORG     = True
  (==) XML     XML     = True
  (==) DOT     DOT     = True
  (==) EDDA    EDDA    = True
  (==) COMPACT COMPACT = True
  (==) IDRIS   IDRIS   = True
  (==) STRING  STRING  = True
  (==) _       _       = False

instance Show SifOutFormat where
  show LATEX   = "LaTeX"
  show CMARK   = "CommonMark"
  show ORG     = "Org"
  show XML     = "XML"
  show DOT     = "dot"
  show EDDA    = "Edda"
  show COMPACT = "Compact"
  show IDRIS   = "Idris"
  show STRING  = "String"

readOutFMT : String -> Maybe SifOutFormat
readOutFMT s =
  case toLower s of
    "latex"    => Just LATEX
    "markdown" => Just CMARK
    "org"      => Just ORG
    "xml"      => Just XML
    "dot"      => Just DOT
    "compact"  => Just COMPACT
    "string"   => Just STRING
    "idris"    => Nothing -- TODO Just IDRIS
    "edda"     => Nothing -- TODO Just EDDA
    otherwise  => Nothing


data SifDomain = MkDomain String (Maybe String)

defaultDomain : SifDomain
defaultDomain = MkDomain "Default" (Just "Not Specified")

instance Eq SifDomain where
  (==) (MkDomain x xd) (MkDomain y yd) = x == y && xd == yd


-- --------------------------------------------------------------------- [ EOF ]
