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

data LTy = PATTERN PTy | RELATION | AFFECT | REQUIREMENT

using (gam : List LTy)

  data HasType : List LTy -> LTy -> Type where
    Here  : HasType (t::ts) t
    There : HasType ts t -> HasType (t'::ts) t

  data Decl : List LTy -> LTy -> Type where
-- -------------------------------------------------------- [ PLang Constructs ]
    Var : HasType gam t  -> Decl gam t

    Functional     : String -> Decl gam REQUIREMENT
    Usability      : String -> Decl gam REQUIREMENT
    Reliability    : String -> Decl gam REQUIREMENT
    Performance    : String -> Decl gam REQUIREMENT
    Supportability : String -> Decl gam REQUIREMENT

    Component : String -> Decl gam (PATTERN COMPONENT)
    System    : String -> Decl gam (PATTERN SYSTEM)
    Generic   : String -> Decl gam (PATTERN GENERIC)
    Deploy    : String -> Decl gam (PATTERN DEPLOY)
    Admin     : String -> Decl gam (PATTERN ADMIN)
    Code      : String -> Decl gam (PATTERN CODE)

    Provides : Decl gam (PATTERN ty) -> Contrib -> Decl gam REQUIREMENT -> Decl gam AFFECT
    Affects  : Decl gam (PATTERN ty) -> Contrib -> Decl gam REQUIREMENT -> Decl gam AFFECT

    LinkedTo    : Decl gam (PATTERN x) -> Decl gam (PATTERN y) -> Decl gam RELATION
    Implements  : Decl gam (PATTERN x) -> Decl gam (PATTERN y) -> {auto prf : ValidR x y} -> Decl gam RELATION
    Uses        : Decl gam (PATTERN x) -> Decl gam (PATTERN y) -> {auto prf : ValidU x y} -> Decl gam RELATION
    Specialises : Decl gam (PATTERN x) -> Decl gam (PATTERN y) -> {auto prf : ValidI x y} -> Decl gam RELATION

    -- MkNode : Decl gam (PATTERN x) -> Decl gam PNODE

    -- MkLang : List (Decl gam REQUIREMENT)
    --       -> List (Decl gam PNODE)
    --       -> List (Decl gam AFFECT)
    --       -> List (Decl gam RELATION)
    --       -> Decl gam LANG

-- ------------------------------------------------------ [ Control Constructs ]
  data Stmt : List LTy -> Type where
    Dcl : Decl gam t -> Stmt gam
    Let : Decl gam t -> Stmt (t::gam) -> Stmt gam
    Seq : Stmt gam -> Stmt gam -> Stmt gam

-- ---------------------------------------------------------------- [ Notation ]
  mkLet : TTName -> Decl gam t -> Stmt (t::gam) -> Stmt gam
  mkLet _ expr body = Let expr body

  dsl sif
    variable    = id
    index_first = Here
    index_next  = There
    let         = mkLet

  implicit
  convVar : (ix : HasType gam t) -> Decl gam t
  convVar = Var

  (>>=) : Stmt gam -> (() -> Stmt gam) -> Stmt gam
  (>>=) a b = Seq a (b ())

  syntax "PLang" = {gam : List LTy} -> Decl gam LANG
  syntax "Pattern" [x] = {gam : List LTy} -> Decl gam (PATTERN x)

-- --------------------------------------------------------------------- [ EOF ]
