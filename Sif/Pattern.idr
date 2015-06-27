module Sif.Pattern

import public Data.Sigma.DList
import public GRL.Lang.GLang

%access public
%default total

-- ------------------------------------------------------- [ Requirement Types ]

data FTy = FuncTy | UsabTy  | ReliTy | PerfTy | SuppTy

data Requirement : GLang ELEM -> FTy -> Type where
  MkFunctional     : (s : String) -> Requirement (MkGoal s Nothing) FuncTy
  MkUsability      : (s : String) -> Requirement (MkGoal s Nothing) UsabTy
  MkReliability    : (s : String) -> Requirement (MkGoal s Nothing) ReliTy
  MkPerformaance   : (s : String) -> Requirement (MkGoal s Nothing) PerfTy
  MkSupportability : (s : String) -> Requirement (MkGoal s Nothing) SuppTy

data Reqs : List (GLang ELEM) -> Type where
  Nil  : Reqs Nil
  (::) : Requirement x ty -> Reqs xs -> Reqs (x::xs)

data Problem : List (GLang ELEM) -> Type where
  MkProblem : String -> Reqs rs -> Problem rs

addRequirement : Requirement x ty -> Problem xs -> Problem (x::xs)
addRequirement r (MkProblem d rs) = MkProblem d (r::rs)


private
mkTrait : String -> Maybe SValue -> CValue -> GLang ELEM -> (GLang ELEM, GLang INTENT)
mkTrait s m c e = (node, node ==> e | c)
  where
    node : GLang ELEM
    node = MkTask s m

data Trait : (GLang ELEM, GLang INTENT) -> Type where
  MkGood : (s : String)
        -> (m : Maybe SValue)
        -> (c : CValue)
        -> Requirement x fty
        -> Trait (mkTrait s m c x)
  MkBad : (s : String)
        -> (m : Maybe SValue)
        -> (c : CValue)
        -> Requirement x fty
        -> Trait (mkTrait s m c x)


data Traits : List (GLang ELEM) -> List (GLang INTENT) -> Type where
  Nil  : Traits Nil Nil
  (::) : Trait (x,y) -> Traits xs ys -> Traits (x::xs) (y::ys)


data Property : DList GTy GLang gs -> Type where
  MkProperty : String -> Traits es gs -> Property (getProof fromList es ++ gs)

data PS : List (GLang ELEM) -> List (GLang INTENT) -> Type where

data Solution :

-- --------------------------------------------------------------------- [ EOF ]
