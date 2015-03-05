||| Representing Design Patterns..
|||
||| Intuitively, a solution is a list of acts that affect the
||| requirements of the problem.
|||
||| The list is the crowbar attached to the mighty hammer I am using
||| to make this work.
module Sif.Pattern.Solution

import public GRL
import public Sif.Pattern.Problem

||| Within the 'Solution' EDSL there are several types of element.
data STy = ACTION | RELATION | PROPERTY | SPEC

using (m : GModel MODEL, p : Problem m PSPEC)
  mutual

    ||| A design pattern is indexed over a problem specification, a
    ||| corresponding GRL model and a type.
    data Pattern : Problem m PSPEC -> GModel ty -> STy -> Type where

      ||| A thing that a property does to resolve a force.
      Action : (name : Maybe String)
             -> (evalue : EvalVal)
             -> Pattern p (Task name evalue) ACTION

      ||| Actions can have sub requirements.
      HasSubAction : (x : Pattern p a ACTION)
                  -> (rty : DTy)
                  -> (y : Pattern p b ACTION)
                  -> Pattern p (genDComp rty a b) RELATION

      ||| Actions will act upon requirements from the problem.
      ActsOn : (a : Pattern p x ACTION)
             -> (c : Contrib)
             -> (f : Problem g FORCE)
--             -> {auto prf : usesForce f p = Yes prf'}
             -> Pattern p (Impacts c x g) RELATION

      ||| Use of acts will affect other acts.
      SideEffect : (a : Pattern p x ACTION)
                -> (c : Contrib)
                -> (b : Pattern p y ACTION)
                -> Pattern p (Effects c x y) ACTION

      ||| Properties are aspects of a solution that will affect
      ||| several forces in the problem.
      Property : (name : Maybe String)
              -> (eval : EvalVal)
              -> (actions : Actions as p)
              -> (links : Relations rs p)
              -> Pattern p (GRLSpec gas ges) PROPERTY

      ||| Construct a pattern.
      MkPattern : (title : Maybe String)
                -> (props : Properties ps p)
                -> Pattern p (foldGRLS m ps) SPEC

  -- --------------------------------------------------------- [ Helpers Special ]
    namespace Actions
      data Actions : List (GModel ELEM) -> Problem m PSPEC -> Type where
        Nil : Actions Nil p
        (::) : Pattern p e ACTION -> Actions es p -> Actions (e::es) p

    namespace Relations
      data Relations : List (GModel LINK) -> Problem m PSPEC -> Type where
        Nil : Relations Nil p
        (::) : Pattern p e RELATION -> Relations es p -> Relations (e::es) p

    namespace Properties
      data Properties : List (GModel MODEL) -> Problem m PSPEC -> Type where
        Nil  : Properties Nil p
        (::) : Pattern p e PROPERTY -> Properties es p -> Properties (e::es) p

  -- --------------------------------------------------------------------- [ EOF ]
