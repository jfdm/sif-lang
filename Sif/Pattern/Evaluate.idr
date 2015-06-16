module Sif.Evaluate

import public Effects
import public Effect.State
import public Effect.Exception

import Sif.Pattern.Problem
import Sif.Pattern.Solution

mutual
  evalList : List (Sif ty) -> {[STATE (List (Sif FORCE))]} Eff ()
  evalList Nil = pure ()
  evalList (x::xs) = do
   eval x
   evalList xs

  eval : Sif ty -> {[STATE (List (Sif FORCE))]} Eff ()
  eval (Solution _ ps) = evalList ps
  eval (Property _ fs) = evalList fs
  eval (Act _ c f)     = do
    env <- get
    put env
  eval _ = pure ()


evalPattern : Sif PATTERN -> {[STATE (List (Sif FORCE))]} Eff $ List (Sif FORCE)
evalPattern (Pattern title (Problem d fs) sol) = do
    put fs
    eval sol
    pure !get
