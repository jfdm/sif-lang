-- ----------------------------------------------------------------- [ Lib.idr ]
-- Module    : Lib.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A library of Patterns
module Sif.Lib

import Sif.Pattern

import public Sif.Lib.Pattern.Secrecy.AsymCrypto
import public Sif.Lib.Pattern.Secrecy.SymmCrypto
-- import public Sif.Lib.Pattern.AccessControl.ABAC
-- import public Sif.Lib.Pattern.AccessControl.Authorisation
import public Sif.Lib.Pattern.AccessControl.ReferenceMonitor

library : List PATTERN
library = [ infoSecAsymCrypto
          , infoSecSymmCrypto
          , referenceMonitor
          ]

getIndicies : List PATTERN -> List (Nat, String)
getIndicies xs = doGet Z xs
  where
    doGet : Nat -> List PATTERN -> List (Nat, String)
    doGet c Nil = Nil
    doGet c (x::xs) = (c, getPatternTitle x) :: doGet c xs
-- --------------------------------------------------------------------- [ EOF ]
