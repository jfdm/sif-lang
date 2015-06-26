module Sif.Pattern

import public Sif.PModel

%access public
%default total

-- No need to specify that links and subs are for requirements, GRL
-- takes care of that through existence proof.

infixl 4 +=
-- ---------------------------------------------------------------- [ Problems ]

record Problem  where
  constructor MkProblem
  name   : String
  forces : DList FTy       (\ty => PModel (Req ty)  ELEM)   fs
  links  : DList LTy       (\ty => PModel (Link ty) INTENT) ls
  joins  : DList GStructTy (\ty => PModel (Sub ty)  STRUCT) ss


private
buildProblem : Problem -> GModel
buildProblem (MkProblem d fs ls ss) =
  DList.foldl (flip $ insert) m' ss
    where
      m : GModel
      m = DList.foldl (flip $ insert) emptyModel fs

      m' : GModel
      m' = DList.foldl (flip $ insert) m ls

namespace Problem
    data ValidInsert : PMTy -> Type where
      E : ValidInsert (Req x)
      L : ValidInsert (Link x)
      S : ValidInsert (Sub x)

    empty : String -> Problem
    empty s = MkProblem s Nil Nil Nil

    insert : PModel x ty -> {auto prf : ValidInsert x} -> Problem -> Problem
    insert {ty=ELEM}   {x=Req y}  e p = record {forces = e :: forces p} p
    insert {ty=INTENT} {x=Link y} l p = record {links  = l :: links p } p
    insert {ty=STRUCT} {x=Sub y}  s p = record {joins  = s :: joins p}  p
    insert _ p = p

    (+=) : Problem -> PModel x ty -> {auto prf : ValidInsert x} -> Problem
    (+=) p e = insert e p

-- ------------------------------------------------------- [ Solution Building ]

private
record SubModel  where
  constructor MkSubModel
  name  : String
  acts  : DList ATy       (\ty => PModel (Act ty)  ELEM)   fs
  links : DList LTy       (\ty => PModel (Link ty) INTENT) ls
  joins : DList GStructTy (\ty => PModel (Sub ty)  STRUCT) ss



private
addSubModel : SubModel -> GModel -> GModel
addSubModel (MkSubModel d as ls ss) g =
  DList.foldl (flip $ insert) m' ss
    where
      m : GModel
      m = DList.foldl (flip $ insert) g as

      m' : GModel
      m' = DList.foldl (flip $ insert) m ls


private
mergeSubModels : SubModel -> SubModel -> SubModel
mergeSubModels (MkSubModel i a b c) (MkSubModel j x y z) =
    MkSubModel (i ++ j) (a ++ x) (b ++ y) (c ++ z)

Property : Type
Property = SubModel

namespace SubModel
  empty : String -> SubModel
  empty s = MkSubModel s Nil Nil Nil

  data ValidSMInsert : PMTy -> Type where
    E : ValidSMInsert (Act x)
    L : ValidSMInsert (Link x)
    S : ValidSMInsert (Sub x)

  update : PModel x ty -> {auto prf : ValidSMInsert x} -> SubModel -> SubModel
  update {ty=ELEM}   {x=Act y}  e p = record {acts = e :: acts p} p
  update {ty=INTENT} {x=Link y} l p = record {links  = l :: links p } p
  update {ty=STRUCT} {x=Sub y}  s p = record {joins  = s :: joins p}  p
  update _ p = p

  (+=) : SubModel -> PModel x ty -> {auto prf : ValidSMInsert x} -> SubModel
  (+=) p e = update e p



record Solution where
  constructor MkSolution
  name  : String
  props : List Property
  acts  : DList ATy       (\ty => PModel (Act ty)  ELEM)   as
  links : DList LTy       (\ty => PModel (Link ty) INTENT) ls
  joins : DList GStructTy (\ty => PModel (Sub ty)  STRUCT) ss

namespace Solution
  empty : String -> Solution
  empty s = MkSolution s Nil Nil Nil Nil

  addExpr : PModel x ty -> {auto prf : ValidSMInsert x} -> Solution -> Solution
  addExpr {ty=ELEM}   {x=Act y}  e p = record {acts = e :: acts p} p
  addExpr {ty=INTENT} {x=Link y} l p = record {links  = l :: links p } p
  addExpr {ty=STRUCT} {x=Sub y}  s p = record {joins  = s :: joins p}  p
  addExpr _ p = p


  (+=) : Solution -> PModel x ty -> {auto prf : ValidSMInsert x} -> Solution
  (+=) p e = addExpr e p

namespace Property

  addProperty : Property -> Solution -> Solution
  addProperty p s = record {props = p :: props s} s

  (+=) : Solution -> Property -> Solution
  (+=) s p = addProperty p s


record Pattern where
  constructor MkPattern
  name : String
  prob : Problem
  sol  : Solution


-- Gives option to augment the property building.
private
buildProperty : Property -> SubModel
buildProperty (MkSubModel d as is ss) = MkSubModel d as is ss

private
buildSolution : Solution -> SubModel
buildSolution (MkSolution d ps as is ss) =
  foldr (\p,i => mergeSubModels (buildProperty p) i) (MkSubModel d as is ss) ps


buildPattern : Pattern -> GModel
buildPattern (MkPattern d p s) = addSubModel i $ buildProblem p
  where
    i : SubModel
    i = buildSolution s
