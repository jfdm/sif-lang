-- ----------------------------------------------------------------- [ Lib.idr ]
-- Module    : Lib.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| A library of Patterns
module Sif.Lib

import Effect.Default

import Sif.Pattern

import public Sif.Lib.Problem
import public Sif.Lib.Solution
import public Sif.Lib.Pattern

record SifLib where
  constructor MkSLib
  patts : List PATTERN
  probs : List PROBLEM
  solts : List SOLUTION

defaultLib : SifLib
defaultLib = MkSLib xs ys zs
  where
    xs : List PATTERN
    xs = [ infoSecAsymCrypto
         , infoSecSymmCrypto
         , referenceMonitor]

    ys : List PROBLEM
    ys = [ accessControl
         , infosec
         , policyEnforcement]

    zs : List SOLUTION
    zs = [ symCrypto
         , asymCrypto
         , refmon]

emptyLib : SifLib
emptyLib = MkSLib Nil Nil Nil

instance Default SifLib where
  default = defaultLib

cmbState : SifLib -> SifLib -> SifLib
cmbState (MkSLib a b c) (MkSLib a' b' c') =
  MkSLib (a++a') (b++b') (c++c')

getIndex : String -> (a -> String) -> List a -> List (Nat, String)
getIndex s f xs = doGet Z xs
  where
    doGet : Nat -> List a -> List (Nat, String)
    doGet _ Nil = Nil
    doGet c (x::xs) = (c, unwords [s, ":", f x]) :: doGet (S c) xs

getPatternIndex : SifLib -> List (Nat, String)
getPatternIndex (MkSLib ps _ _) =
    getIndex "Pattern" (\x => getPatternTitle x) ps

getSolutionIndex : SifLib -> List (Nat, String)
getSolutionIndex (MkSLib _ _ ss) =
    getIndex "Solution" (\x => getSolutionTitle x) ss

getProblemIndex : SifLib -> List (Nat, String)
getProblemIndex (MkSLib _ ps _) =
    getIndex "Problem" (\x => getProblemTitle x) ps

-- --------------------------------------------------------------------- [ EOF ]
