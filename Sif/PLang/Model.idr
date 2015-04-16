module Sif.PLang.Model

import GRL.Model

import Sif.PLang.Common

mutual
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

    Provides : PLang a (PATTERN ty) -> (c : Contrib) -> PLang b REQUIREMENT -> PLang (Impacts c a b) AFFECT
    Affects  : PLang a (PATTERN ty) -> (c : Contrib) -> PLang b REQUIREMENT -> PLang (Effects c a b) AFFECT

    LinkedTo    : PLang a (PATTERN x) -> PLang b (PATTERN y) -> PLang (Impacts ZERO a b) RELATION
    Implements  : PLang a (PATTERN x) -> PLang b (PATTERN y) -> {auto prf : ValidR x y} -> PLang (AND a [b]) RELATION
    Uses        : PLang a (PATTERN x) -> PLang b (PATTERN y) -> {auto prf : ValidU x y} -> PLang (AND a [b]) RELATION
    Specialises : PLang a (PATTERN x) -> PLang b (PATTERN y) -> {auto prf : ValidI x y} -> PLang (AND a [b]) RELATION


    MkPLang : List (PLang REQUIREMENT)
           -> Patterns
           -> List (PLang AFFECT)
           ->

  namespace Patterns
    data Patterns : List (GModel ELEM) -> Type where
      Nil  : Patterns Nil
      (::) : PLang e (PATTERN x) -> Patterns es -> Patterns (e :: es)

  namespace Requirements
    data Requirements : List (GModel ELEM) -> Type where
      Nil  : Requirements Nil
      (::) : PLang r REQUIREMENT -> Requirements rs -> Requirements (r::rs)

  namespace Affects
    data Affects : List (GModel LINK) -> Type where
      Nil  : Affects Nil
      (::) : PLang a AFFECT -> Affects as -> Affects (a :: as)
