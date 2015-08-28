-- ----------------------------------------------------------------- [ Lib.idr ]
-- Module    : Lib.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A library of Patterns
module Sif.Library

import Effect.Default
import Config.YAML

import Sif.Types
import Sif.Pattern
import Sif.Builder.AbsInterp

import public Data.Sigma.DList
import public Data.AVL.Dict

-- -------------------------------------------------------------- [ Directives ]
%access public

-- --------------------------------------------------------- [ Data Structures ]

record LibEntry (impl : SifTy -> Type) where
  constructor MkEntry
  idx   : Nat
  entry : PATTERN impl

record SifLib where
  constructor MkSLib
  counter : Nat
  patts   : DList (SifTy -> Type) LibEntry is

emptyLib : SifLib
emptyLib = MkSLib Z Nil

addToLibrary : PATTERN impl -> SifLib -> SifLib
addToLibrary p (MkSLib c ps) = MkSLib (S c) (MkEntry c p::ps)

addToLibraryM : List (PATTERN impl) -> SifLib -> SifLib
addToLibraryM xs lib = foldl (flip $ addToLibrary) lib xs

defaultLib : SifLib
defaultLib = emptyLib

instance Default SifLib where
  default = defaultLib

getLibraryIndex : SifLib -> Dict Nat String
getLibraryIndex lib = Dict.fromList idx
  where
    f : LibEntry impl -> (Nat, String)
    f (MkEntry n p) = (n, unwords ["Pattern:", fromMaybe "" $ Pattern.getTitle p])

    idx : List (Nat, String)
    idx = mapDList f (patts lib)

getPatternByIndex : Nat -> SifLib -> Maybe (impl ** PATTERN impl)
getPatternByIndex n lib =
  case index n (patts lib) of -- Possibly reverse index... or use find on idx on rentry
    Nothing       => Nothing
    Just (_ ** p) => Just (_ ** entry p)

indexToString : Dict Nat String -> String
indexToString idx = unlines res
  where
    itemToString : (Nat, String) -> List String -> List String
    itemToString (i,t) rs = (unwords [show i, "<--", t]) :: rs

    res : List String
    res = foldr (itemToString) Nil $ Dict.toList idx

-- --------------------------------------------------------------------- [ EOF ]
