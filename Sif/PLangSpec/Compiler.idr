module Sif.PLangSpec.Compiler

import public Effects
import public Effect.State
import public Effect.Exception

import public GRL

import Sif.PLangSpec.Definition


Value : LTy -> Type
Value (PATTERN _) = GModel ELEM
Value RELATION    = GModel LINK
Value AFFECT      = GModel LINK
Value REQUIREMENT = GModel ELEM

-- ------------------------------------------------------------------ [ Memory ]

data Env : List LTy -> Type where
 Nil  : Env Nil
 (::) : Value t -> Env ts -> Env (t :: ts)

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

covering
evalDecl : Env gam -> Decl gam t -> Value t
evalDecl env (Var x) = read x env

evalDecl env (Functional     n) = Goal (Just n) UNKNOWN
evalDecl env (Usability      n) = Goal (Just n) UNKNOWN
evalDecl env (Reliability    n) = Goal (Just n) UNKNOWN
evalDecl env (Performance    n) = Goal (Just n) UNKNOWN
evalDecl env (Supportability n) = Goal (Just n) UNKNOWN

evalDecl env (Provides a c b) = Effects c (evalDecl env a) (evalDecl env b)
evalDecl env (Affects a c b)  = Impacts c (evalDecl env a) (evalDecl env b)

evalDecl env (Component n) = Task (Just n) UNKNOWN
evalDecl env (System    n) = Task (Just n) UNKNOWN
evalDecl env (Generic   n) = Task (Just n) UNKNOWN
evalDecl env (Deploy    n) = Task (Just n) UNKNOWN
evalDecl env (Admin     n) = Task (Just n) UNKNOWN
evalDecl env (Code      n) = Task (Just n) UNKNOWN

-- unless rs are folded potential bottle neck later on.
evalDecl env (Implements  a b) = AND (evalDecl env a) [(evalDecl env b)]
evalDecl env (LinkedTo    a b) = AND (evalDecl env a) [(evalDecl env b)]
evalDecl env (Uses        a b) = AND (evalDecl env a) [(evalDecl env b)]
evalDecl env (Specialises a b) = AND (evalDecl env a) [(evalDecl env b)]

doInterp : Env gam
        -> Stmt gam
        -> GModel MODEL
        -> (Env gam, GModel MODEL)
doInterp env (Dcl expr) g =
  let d = (evalDecl env expr) in (env, insertIntoGRL d g)

doInterp env (Let expr body) g =
  let d           = evalDecl expr         in
  let env'        = alloc d env           in
  let g'          = insertIntoGRL d g     in
  let (env'', g') = doInterp env' body g' in (free env'', insertIntoGRL d' g')

doInterp env (Seq x y) g =
  let (env', g') = doInterp env x g      in
  let g''        = insertIntoGRL g' g    in
  let (env'', y') = doInterp env' y g''  in (env'', insertIntoGRL y' g')

interp : Stmt [] -> GModel MODEL
interp ss = snd $ doInterp Nil ss (GRLSpec Nil Nil)
-- --------------------------------------------------------------------- [ EOF ]
