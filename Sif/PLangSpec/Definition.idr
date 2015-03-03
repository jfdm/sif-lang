||| Language definition
module Sif.PLangSpec.Definition

import public GRL

-- ------------------------------------------------------------------- [ Types ]
data PTy = COMPONENT | SYSTEM | GENERIC | DEPLOY | ADMIN | CODE

-- -------------------------------------------------------------- [ Predicates ]

data ValidR : PTy -> PTy -> Type where
  RealCC : ValidR COMPONENT COMPONENT
  RealCG : ValidR COMPONENT GENERIC
  RealGG : ValidR GENERIC   GENERIC
  RealIC : ValidR CODE      COMPONENT
  RealIG : ValidR CODE      GENERIC

data ValidI : PTy -> PTy -> Type where
  SpeciSS : ValidI SYSTEM    SYSTEM
  SpeciDS : ValidI DEPLOY    SYSTEM
  SpeciCC : ValidI COMPONENT COMPONENT
  SpeciCG : ValidI COMPONENT GENERIC
  SpeciGG : ValidI GENERIC   GENERIC

data ValidU : PTy -> PTy -> Type where
  UsesCC : ValidU COMPONENT COMPONENT
  UsesCP : ValidU COMPONENT GENERIC
  UsesSS : ValidU SYSTEM    SYSTEM
  UsesSD : ValidU SYSTEM    DEPLOY
  UsesSC : ValidU SYSTEM    COMPONENT
  UsesSA : ValidU SYSTEM    ADMIN
  UsesSP : ValidU SYSTEM    GENERIC
  UsesII : ValidU CODE      CODE
  UsesPP : ValidU GENERIC   GENERIC

data LTy = PATTERN PTy | RELATION | AFFECT | REQUIREMENT | LANG | PNODE

using (G : List LTy, G' : List LTy)

  data HasType : List LTy -> LTy -> Type where
    Here  : HasType (t::ts) t
    There : HasType ts t -> HasType (t'::ts) t

  data Expr : List LTy -> LTy -> Type where
-- -------------------------------------------------------- [ PLang Constructs ]
    Var : HasType G t  -> Expr G t

    Functional     : String -> Expr G REQUIREMENT
    Usability      : String -> Expr G REQUIREMENT
    Reliability    : String -> Expr G REQUIREMENT
    Performance    : String -> Expr G REQUIREMENT
    Supportability : String -> Expr G REQUIREMENT

    Component : String -> Expr G (PATTERN COMPONENT)
    System    : String -> Expr G (PATTERN SYSTEM)
    Generic   : String -> Expr G (PATTERN GENERIC)
    Deploy    : String -> Expr G (PATTERN DEPLOY)
    Admin     : String -> Expr G (PATTERN ADMIN)
    Code      : String -> Expr G (PATTERN CODE)

    Effects  : Expr G (PATTERN ty) -> Contrib -> Expr G REQUIREMENT -> Expr G AFFECT
    Impacts  : Expr G (PATTERN ty) -> Contrib -> Expr G REQUIREMENT -> Expr G AFFECT

    LinkedTo    : Expr G (PATTERN x) -> Expr G (PATTERN y) -> Expr G RELATION
    Implements  : Expr G (PATTERN x) -> Expr G (PATTERN y) -> {auto prf : ValidR x y} -> Expr G RELATION
    Uses        : Expr G (PATTERN x) -> Expr G (PATTERN y) -> {auto prf : ValidU x y} -> Expr G RELATION
    Specialises : Expr G (PATTERN x) -> Expr G (PATTERN y) -> {auto prf : ValidI x y} -> Expr G RELATION

    MkNode : Expr G (PATTERN x) -> Expr G PNODE

    MkLang : List (Expr G REQUIREMENT)
          -> List (Expr G PNODE)
          -> List (Expr G AFFECT)
          -> List (Expr G RELATION)
          -> Expr G LANG

-- ------------------------------------------------------ [ Control Constructs ]
    Let    : Expr G t -> Expr (t::G) t' -> Expr G t'
    Bind   : Expr G a -> Expr G b -> Expr G b

-- ---------------------------------------------------------------- [ Notation ]
  mkLet : TTName -> Expr G t -> Expr (t::G) t' -> Expr G t'
  mkLet _ expr body = Let expr body

  dsl sif
    variable    = id
    index_first = Here
    index_next  = There
    let         = mkLet

  implicit
  convVar : (ix : HasType G t) -> Expr G t
  convVar = Var

  (>>=) : Expr G a -> (() -> Expr G b) -> Expr G b
  (>>=) a b = Bind a (b ())

  syntax "PLang" = {G : List LTy} -> Expr G LANG
  syntax "Pattern" [x] = {G : List LTy} -> Expr G (PATTERN x)

-- --------------------------------------------------------------------- [ EOF ]
