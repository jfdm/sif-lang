module Sif.PLangSpec.Compiler

import public Effects
import public Effect.State
import public Effect.Exception

import public GRL

import Sif.PLangSpec.Definition


using (G : List LTy)

  Value : LTy -> Type
  Value LANG        = GModel MODEL
  Value (PATTERN _) = GModel ELEM
  Value PNODE       = GModel ELEM
  Value RELATION    = GModel LINK
  Value AFFECT      = GModel LINK
  Value REQUIREMENT = GModel ELEM

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

  compile (Component n) = pure $ Task (Just n) UNKNOWN
  compile (System    n) = pure $ Task (Just n) UNKNOWN
  compile (Generic   n) = pure $ Task (Just n) UNKNOWN
  compile (Deploy    n) = pure $ Task (Just n) UNKNOWN
  compile (Admin     n) = pure $ Task (Just n) UNKNOWN
  compile (Code      n) = pure $ Task (Just n) UNKNOWN

  compile (MkNode p) = compile p

  -- unless rs are folded potential bottle neck later on.
  compile (Implements  a b) = pure $ AND !(compile a) [!(compile b)]
  compile (LinkedTo    a b) = pure $ AND !(compile a) [!(compile b)]
  compile (Uses        a b) = pure $ AND !(compile a) [!(compile b)]
  compile (Specialises a b) = pure $ AND !(compile a) [!(compile b)]

  compile (MkLang fs ps as rs) = do
      fs' <- mapE (compile) fs
      ps' <- mapE (compile) ps
      as' <- mapE (compile) as
      rs' <- mapE (compile) rs
      pure $ GRLSpec (fs' ++ ps') (as' ++ rs')

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
