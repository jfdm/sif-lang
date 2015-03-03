module Sif.PLangSpec.Compiler

import public Effects
import public Effect.State
import public Effect.Exception

import public GRL

import Sif.PLangSpec.Definition


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

read : HasType gam t -> Env gam -> Value t
read Here      (val :: store) = val
read (There x) (val :: store) = read x store

write : HasType gam t -> Value t -> Env gam -> Env gam
write Here      val (_ :: store)    = val :: store
write (There x) val (val' :: store) = val' :: write x val store

alloc : Value t -> Env gam -> Env (t::gam)
alloc = (::)

free : Env (t::gam) -> Env gam
free (_::store) = store

-- ----------------------------------------------------- [ Construct gamRL Model ]

namespace Decls
  data DList : Type where
    Nil  : DList
    (::) : GModel ty -> DList -> DList

  dApp : DList -> DList -> DList
  dApp Nil ys = ys
  dApp (x::xs) ys = x :: dApp xs ys

  instance Default DList where
    default = Nil

CompileEffs : List LTy -> List EFFECT
CompileEffs gam = [STATE (Env gam)]

covering
evalDecl : Decl gam t -> {[STATE (Env gam)]} Eff $ Value t
evalDecl (Var x) = pure $ read x !(get)

evalDecl (Functional     n) = pure $ Goal (Just n) UNKNOWN
evalDecl (Usability      n) = pure $ Goal (Just n) UNKNOWN
evalDecl (Reliability    n) = pure $ Goal (Just n) UNKNOWN
evalDecl (Performance    n) = pure $ Goal (Just n) UNKNOWN
evalDecl (Supportability n) = pure $ Goal (Just n) UNKNOWN

evalDecl (Provides a c b) = pure $ Effects c !(evalDecl a) !(evalDecl b)
evalDecl (Affects a c b)  = pure $ Impacts c !(evalDecl a) !(evalDecl b)

evalDecl (Component n) = pure $ Task (Just n) UNKNOWN
evalDecl (System    n) = pure $ Task (Just n) UNKNOWN
evalDecl (Generic   n) = pure $ Task (Just n) UNKNOWN
evalDecl (Deploy    n) = pure $ Task (Just n) UNKNOWN
evalDecl (Admin     n) = pure $ Task (Just n) UNKNOWN
evalDecl (Code      n) = pure $ Task (Just n) UNKNOWN

-- unless rs are folded potential bottle neck later on.
evalDecl (Implements  a b) = pure $ AND !(evalDecl a) [!(evalDecl b)]
evalDecl (LinkedTo    a b) = pure $ AND !(evalDecl a) [!(evalDecl b)]
evalDecl (Uses        a b) = pure $ AND !(evalDecl a) [!(evalDecl b)]
evalDecl (Specialises a b) = pure $ AND !(evalDecl a) [!(evalDecl b)]

covering
interp' : Stmt gam -> {CompileEffs gam} Eff $ DList
interp' (Dcl decl) = do
  d <- evalDecl decl
  pure [d]

interp' (Let expr body) = do
  d <- evalDecl expr
  updateM (\xs => alloc d xs)
  ds <- interp' body
  updateM (\xs => free xs)
  pure (dApp [d] ds)

interp' (Seq x y) = do
  xs <- interp' x
  ys <- interp' y
  pure (dApp xs (dApp ys xs))

interp : Stmt gam -> GModel MODEL
interp ss = mkModel (run (interp' ss)) (GRLSpec Nil Nil)
  where

    mkModel : DList -> GModel MODEL -> GModel MODEL
    mkModel Nil g = g
    mkModel (d::ds) g = insertIntoGRL d (mkModel ds g)

-- --------------------------------------------------------------------- [ EOF ]
