module Sif.PLangSpec.Compiler

import public Effects
import public Effect.State
import public Effect.Exception

import public GRL

import Sif.PLangSpec.Definition

interpTy : LTy -> Type
interpTy (PATTERN _) = GModel ELEM
interpTy RELATION    = GModel LINK
interpTy AFFECT      = GModel LINK
interpTy REQUIREMENT = GModel ELEM

-- ------------------------------------------------------------------ [ Memory ]

data Env : List LTy -> Type where
 Nil  : Env Nil
 (::) : interpTy t -> Env ts -> Env (t :: ts)

read : HasType gam t -> Env gam -> interpTy t
read Here      (val :: store) = val
read (There x) (val :: store) = read x store

write : HasType gam t -> interpTy t -> Env gam -> Env gam
write Here      val (_ :: store)    = val :: store
write (There x) val (val' :: store) = val' :: write x val store

alloc : interpTy t -> Env gam -> Env (t::gam)
alloc = (::)

free : Env (t::gam) -> Env gam
free (_::store) = store

-- ----------------------------------------------------- [ Construct gamRL Model ]

covering
evalDecl : Env gam -> Decl gam t -> (Env gam, interpTy t)
evalDecl {t} env (Var x) = (env, read x env)

evalDecl env (Functional     n) = (env, Goal (Just n) UNKNOWN)
evalDecl env (Usability      n) = (env, Goal (Just n) UNKNOWN)
evalDecl env (Reliability    n) = (env, Goal (Just n) UNKNOWN)
evalDecl env (Performance    n) = (env, Goal (Just n) UNKNOWN)
evalDecl env (Supportability n) = (env, Goal (Just n) UNKNOWN)

evalDecl env (Provides a c b) = (env, Effects c (snd $ evalDecl env a) (snd $ evalDecl env b))
evalDecl env (Affects a c b)  = (env, Impacts c (snd $ evalDecl env a) (snd $ evalDecl env b))

evalDecl env (Component n) = (env, Task (Just n) UNKNOWN)
evalDecl env (System    n) = (env, Task (Just n) UNKNOWN)
evalDecl env (Generic   n) = (env, Task (Just n) UNKNOWN)
evalDecl env (Deploy    n) = (env, Task (Just n) UNKNOWN)
evalDecl env (Admin     n) = (env, Task (Just n) UNKNOWN)
evalDecl env (Code      n) = (env, Task (Just n) UNKNOWN)

-- unless rs are folded potential bottle neck later on.
evalDecl env (Implements  a b) = (env, AND (snd $ evalDecl env a) [(snd $ evalDecl env b)])
evalDecl env (LinkedTo    a b) = (env, AND (snd $ evalDecl env a) [(snd $ evalDecl env b)])
evalDecl env (Uses        a b) = (env, AND (snd $ evalDecl env a) [(snd $ evalDecl env b)])
evalDecl env (Specialises a b) = (env, AND (snd $ evalDecl env a) [(snd $ evalDecl env b)])

covering
interp : Env gam -> Stmt gam -> {[STATE (GModel MODEL)]} Eff (Env gam)
interp env (Dcl expr) = do
    let (env', x) = (evalDecl env expr)
    updateM (\g => insertIntoGRL x g)
    pure env'

interp env (Let expr body) = do
  let (_, x) = evalDecl env expr
  updateM (\g => insertIntoGRL x g)

  let env' = alloc x env
  env'' <- interp env' body
  pure $ free env''

interp env (Seq x y) = do
  env' <- interp env x
  interp env' y

compile : Stmt Nil -> GModel MODEL
compile ss = runPureInit [(GRLSpec Nil Nil)] (do interp Nil ss; get)


-- --------------------------------------------------------------------- [ EOF ]
