-- ----------------------------------------------------------------- [ Lib.idr ]
-- Module    : Lib.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A library of Patterns
module Sif.Lib

import Effect.Default

import Sif.Pattern

import public Data.AVL.Dict
import public Sif.Lib.Problem
import public Sif.Lib.Solution
import public Sif.Lib.Pattern

record SifLib where
  constructor MkSLib
  counter : Nat
  patts   : Dict Nat PATTERN

emptyLib : SifLib
emptyLib = MkSLib Z empty

addToLibrary : PATTERN -> SifLib -> SifLib
addToLibrary p (MkSLib c ps) = MkSLib (S c) (insert c p ps)

addToLibraryM : List PATTERN -> SifLib -> SifLib
addToLibraryM xs lib = foldl (flip $ addToLibrary) lib xs

defaultLib : SifLib
defaultLib = addToLibraryM xs emptyLib
  where
    xs : List PATTERN
    xs = [ infoSecAsymCrypto
         , infoSecSymmCrypto
         , referenceMonitor]

instance Default SifLib where
  default = defaultLib


getLibraryIndex : SifLib -> Dict Nat String
getLibraryIndex lib = Dict.fromList idx
  where
    f : (Nat, PATTERN) -> (Nat, String)
    f (n, p) = (n, unwords ["Pattern:", getPatternTitle p])

    idx : List (Nat, String)
    idx = map f $ Dict.toList (patts lib)


getPatternByIndex : Nat -> SifLib -> Maybe PATTERN
getPatternByIndex n lib = Dict.lookup n (patts lib)

indexToString : Dict Nat String -> String
indexToString idx = unlines res
  where
    itemToString : (Nat, String) -> List String -> List String
    itemToString (i,t) rs = (unwords [show i, "<--", t]) :: rs

    res : List String
    res = foldr (itemToString) Nil $ Dict.toList idx

-- --------------------------------------------------------------------- [ EOF ]
