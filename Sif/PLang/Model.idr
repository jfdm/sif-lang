module Sif.PLang.Model

import Data.SigmaList

import GRL.Model

import Sif.PLang.Common

data PLang : GModel ty -> LTy -> Type where

  Functional     : (s : Maybe String) -> PLang (Goal s UNKNOWN) REQUIREMENT
  Usability      : (s : Maybe String) -> PLang (Goal s UNKNOWN) REQUIREMENT
  Reliability    : (s : Maybe String) -> PLang (Goal s UNKNOWN) REQUIREMENT
  Performance    : (s : Maybe String) -> PLang (Goal s UNKNOWN) REQUIREMENT
  Supportability : (s : Maybe String) -> PLang (Goal s UNKNOWN) REQUIREMENT

  Component : String -> PLang (Task s UNKNOWN) (PATTERN COMPONENT)
  System    : String -> PLang (Task s UNKNOWN) (PATTERN SYSTEM)
  Generic   : String -> PLang (Task s UNKNOWN) (PATTERN GENERIC)
  Deploy    : String -> PLang (Task s UNKNOWN) (PATTERN DEPLOY)
  Admin     : String -> PLang (Task s UNKNOWN) (PATTERN ADMIN)
  Code      : String -> PLang (Task s UNKNOWN) (PATTERN CODE)

  Provides : PLang a (PATTERN ty)
          -> (c : Contrib)
          -> PLang b REQUIREMENT
          -> PLang (Impacts c a b) AFFECT

  Affects  : PLang a (PATTERN ty)
          -> (c : Contrib)
          -> PLang b REQUIREMENT
          -> PLang (Effects c a b) AFFECT

  LinkedTo : PLang a (PATTERN x)
          -> PLang b (PATTERN y)
          -> PLang (Impacts ZERO a b) RELATION

  Implements : PLang a (PATTERN x)
            -> PLang b (PATTERN y)
            -> {auto prf : ValidR x y}
            -> PLang (AND a [b]) RELATION

  Uses : PLang a (PATTERN x)
      -> PLang b (PATTERN y)
      -> {auto prf : ValidU x y}
      -> PLang (AND a [b]) RELATION

  Specialises : PLang a (PATTERN x)
             -> PLang b (PATTERN y)
             -> {auto prf : ValidI x y}
             -> PLang (AND a [b]) RELATION

  MkPLang : (reqs  : SigmaList (GModel ELEM) (\x => PLang x REQUIREMENT) fs)
         -> (patts : SigmaList (GModel ELEM) (\x => PLang x (PATTERN y)) ps)
         -> (affs  : SigmaList (GModel LINK) (\x => PLang x AFFECT) as)
         -> (rels  : SigmaList (GModel LINK) (\x => PLang x RELATION) rs)
         -> PLang (GRLSpec (fs ++ ps) (as ++ rs)) LANG

-- --------------------------------------------------------------------- [ EOF ]
