||| An EDSL for shallow specification of pattern languages.
|||
||| A shallow specification is one in which only problems are
||| specified and their combinations controlled through typing.  This
||| language is intended to be used for specifying languages only.
||| Evaluating languages must make use of the deep version.
module Sif.LangShallow

import public Effects
import public Effect.State
import public Effect.Exception

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

  Value : LTy -> Type
  Value LANG        = GModel MODEL
  Value (PATTERN _) = GModel MODEL -- @TODO Replace with Actors...
  Value PNODE       = GModel MODEL
  Value RELATION    = GModel MODEL
  Value AFFECT      = GModel LINK
  Value REQUIREMENT = GModel ELEM


  data Expr : List LTy -> LTy -> Type where
-- -------------------------------------------------------- [ PLang Constructs ]
    Var : HasType G t  -> Expr G t

    Functional     : String -> Expr G REQUIREMENT
    Usability      : String -> Expr G REQUIREMENT
    Reliability    : String -> Expr G REQUIREMENT
    Performance    : String -> Expr G REQUIREMENT
    Supportability : String -> Expr G REQUIREMENT

    Component : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN COMPONENT)
    System    : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN SYSTEM)
    Generic   : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN GENERIC)
    Deploy    : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN DEPLOY)
    Admin     : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN ADMIN)
    Code      : List (Expr G REQUIREMENT) -> List (Expr G AFFECT) -> Expr G (PATTERN CODE)

    Effects  : Expr G REQUIREMENT -> Contrib -> Expr G REQUIREMENT -> Expr G AFFECT
    Impacts  : Expr G REQUIREMENT -> Contrib -> Expr G REQUIREMENT -> Expr G AFFECT

    LinkedTo    : Expr G (PATTERN x)-> Expr G (PATTERN y) -> Expr G RELATION
    Implements  : Expr G (PATTERN x)-> Expr G (PATTERN y) -> {auto prf : ValidR x y} -> Expr G RELATION
    Uses        : Expr G (PATTERN x)-> Expr G (PATTERN y) -> {auto prf : ValidU x y} -> Expr G RELATION
    Specialises : Expr G (PATTERN x)-> Expr G (PATTERN y) -> {auto prf : ValidI x y} -> Expr G RELATION

    MkPattern : Expr G (PATTERN x) -> Expr G PNODE

    Lang : List (Expr G PNODE) -> List (Expr G RELATION) -> Expr G LANG

-- ------------------------------------------------------ [ Control Constructs ]
    Let    : Expr G t -> Expr (t::G) t' -> Expr G t'
    Bind   : Expr G a -> Expr G b -> Expr G b
--    Return : Value t -> Expr G t

-- ------------------------------------------------------------------ [ Memory ]

  data Env : List LTy -> Type where
   Nil  : Env Nil
   (::) : Value t -> Env ts -> Env (t :: ts)

  instance Default (Env Nil) where
    default = []


  read : HasType G t -> Env G -> Value t
  read Here      (val :: store) = val
  read (There x) (val :: store) = read x store

  write : HasType G t -> Value t -> Env G -> Env G
  write Here      val (_ :: store)    = val :: store
  write (There x) val (val' :: store) = val' :: write x val store

  alloc : Value t -> Env G -> Env (t::G)
  alloc = (::)

  free : Env (t::G) -> Env G
  free (_::store) = store

-- ---------------------------------------------------------------- [ Notation ]
  dsl sif
    variable    = id
    index_first = Here
    index_next  = There
    let         = Let

  implicit
  convVar : (ix : HasType G t) -> Expr G t
  convVar = Var

  (>>=) : Expr G a -> (() -> Expr G b) -> Expr G b
  (>>=) a b = Bind a (b ())

  -- return : Value a -> Expr G a
  -- return val = Return val

  syntax "PLang" = {G : List LTy} -> Expr G LANG
  syntax "Pattern" [x] = {G : List LTy} -> Expr G (PATTERN x)

-- ----------------------------------------------------- [ Construct GRL Model ]
  compile : Expr G t -> {[STATE (Env G)]} Eff $ Value t
  compile (Var x) = pure $ read x !(get)

  compile (Functional     n) = pure $ Goal (Just n) UNKNOWN
  compile (Usability      n) = pure $ Goal (Just n) UNKNOWN
  compile (Reliability    n) = pure $ Goal (Just n) UNKNOWN
  compile (Performance    n) = pure $ Goal (Just n) UNKNOWN
  compile (Supportability n) = pure $ Goal (Just n) UNKNOWN

  compile (Effects a c b) = pure $ Effects c !(compile a) !(compile b)
  compile (Impacts a c b) = pure $ Impacts c !(compile a) !(compile b)

  compile (Component rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)
  compile (System    rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)
  compile (Generic   rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)
  compile (Deploy    rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)
  compile (Admin     rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)
  compile (Code      rs as) = pure $ GRLSpec !(mapE (compile) rs) !(mapE (compile) as)

  compile (MkPattern p) = compile p

  compile (Implements  a b) = pure $ combineGRLs !(compile a) !(compile b)
  compile (LinkedTo    a b) = pure $ combineGRLs !(compile a) !(compile b)
  compile (Uses        a b) = pure $ combineGRLs !(compile a) !(compile b)
  compile (Specialises a b) = pure $ combineGRLs !(compile a) !(compile b)

  compile (Lang ps rs) = do
      ps' <- mapE (compile) ps
      rs' <- mapE (compile) rs
      pure $ foldGRLS (GRLSpec Nil Nil) (ps' ++ rs')

--  compile (Return res) = pure $ res
  compile (Let expr body) = do
    val <- compile expr
    updateM (\xs => alloc val xs)
    bval <- compile body
    updateM (\xs => free xs)
    pure bval

  compile (Bind x y) = do
    compile x
    compile y

-- --------------------------------------------------------------------- [ EOF ]