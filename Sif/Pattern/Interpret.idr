module Sif.Interpret

import public Effects
import public Effect.State
import public Effect.Exception

import Sif.Model
import Sif.Pattern.Problem
import Sif.Pattern.Solution

-- ------------------------------------------------------------------- [ Types ]

interpTy : Ty -> Type
interpTy x  = Sif x

-- ------------------------------------------------------------------ [ Memory ]

namespace SymTable
  data Env : List Ty -> Type where
    Nil  : Env Nil
    (::) : interpTy t -> Env ts -> Env (t :: ts)

  instance Default (Env Nil) where
    default = Nil

  read : HasType g t -> Env g -> interpTy t
  read Here      (val :: store) = val
  read (There x) (val :: store) = read x store

  write : HasType g t -> interpTy t -> Env g -> Env g
  write Here      val (_ :: store)    = val :: store
  write (There x) val (val' :: store) = val' :: write x val store

  alloc : interpTy t -> Env g -> Env (t::g)
  alloc = (::)

  free : Env (t::g) -> Env g
  free (_::store) = store

-- ------------------------------------------------------------- [ Interpreter ]

mutual
  covering
  interpExprList : List (Expr g t) -> {[STATE (Env g)]} Eff $ List $ interpTy t
  interpExprList Nil     = pure Nil
  interpExprList (x::xs) = do
    x' <- interpExpr x
    xs' <- interpExprList xs
    pure (x'::xs')

  ||| Expression Evaluator
  covering
  interpExpr : Expr g t -> {[STATE (Env g)]} Eff $ interpTy t
  interpExpr (Var x)            = pure $ read x !get
  interpExpr (Usability d)      = pure $ Model.Usability NOUT d
  interpExpr (Functional d)     = pure $ Model.Functional NOUT d
  interpExpr (Reliability d)    = pure $ Model.Reliability NOUT d
  interpExpr (Performance d)    = pure $ Model.Performance NOUT d
  interpExpr (Supportability d) = pure $ Model.Supportability NOUT d
  interpExpr (Act d c f)        = pure $ Model.Act c d !(interpExpr f)
  interpExpr (Property d as)    = pure $ Model.Property d !(interpExprList as)
  interpExpr (Problem d fs)     = pure $ Model.Problem d !(interpExprList fs)
  interpExpr (Solution d ps)    = pure $ Model.Solution d !(interpExprList ps)
  interpExpr (Pattern p s)      = pure $ Model.Pattern Nothing !(interpExpr p) !(interpExpr s)

interp : Stmt g t -> {[STATE (Env g)]} Eff $ (interpTy t)
interp (return p) = pure !(interpExpr p)
interp (Let expr body) = do
  v <- interpExpr expr
  putM $ alloc v !get
  res <- interp body
  putM $ free !get
  pure res
