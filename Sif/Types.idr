-- --------------------------------------------------------------- [ Types.idr ]
-- Module    : Types.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Types

import public Freyja.Common

%access public export
-- ----------------------------------------------- [ Problems and Requirements ]

-- Encoding of CONTROL | CODE | ACTION ??
-- Encoding of Categories => Security, Access Control, HCI...


data STy = ABSTRACT | CONCRETE

data SifTy = TyREQ     | TyTRAIT   | TyPROPERTY | TySOLUTION
           | TyPROBLEM | TyPATTERN | TyAFFECTS


data HasMData : SifTy -> Type where
  HMA : HasMData TyREQ
  HMB : HasMData TyPROPERTY
  HMC : HasMData TySOLUTION
  HMD : HasMData TyPROBLEM
  HME : HasMData TyPATTERN
  HMF : HasMData TyTRAIT

data SifOutFormat = ORG  | LATEX   | CMARK | XML    | DOT
                  | EDDA | COMPACT | IDRIS | STRING | FREYJA

Eq SifOutFormat where
  (==) LATEX   LATEX   = True
  (==) CMARK   CMARK   = True
  (==) ORG     ORG     = True
  (==) XML     XML     = True
  (==) DOT     DOT     = True
  (==) EDDA    EDDA    = True
  (==) COMPACT COMPACT = True
  (==) IDRIS   IDRIS   = True
  (==) STRING  STRING  = True
  (==) FREYJA  FREYJA  = True
  (==) _       _       = False

Show SifOutFormat where
  show LATEX   = "LaTeX"
  show CMARK   = "CommonMark"
  show ORG     = "Org"
  show XML     = "XML"
  show DOT     = "dot"
  show EDDA    = "Edda"
  show COMPACT = "Compact"
  show IDRIS   = "Idris"
  show STRING  = "String"
  show FREYJA  = "Freyja"

export
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
    "freyja"   => Just FREYJA
    "idris"    => Nothing -- @TODO Just IDRIS
    "edda"     => Nothing -- internal
    otherwise  => Nothing


data SifDomain = MkDomain String (Maybe String)

export
defaultDomain : SifDomain
defaultDomain = MkDomain "Default" (Just "Not Specified")

Eq SifDomain where
  (==) (MkDomain x xd) (MkDomain y yd) = x == y && xd == yd

Show SifDomain where
  show (MkDomain x xd) = unwords ["MkDomain", show x, show xd]
-- --------------------------------------------------------------------- [ EOF ]
