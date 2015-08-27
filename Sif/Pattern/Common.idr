-- -------------------------------------------------------------- [ Common.idr ]
-- Module    : Common.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Common

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
           | tyPROBLEM | tyPATTERN | tyTRAITend

-- --------------------------------------------------------------------- [ EOF ]
