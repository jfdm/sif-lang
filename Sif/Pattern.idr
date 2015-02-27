||| Representing Design Patterns..
|||
||| Intuitively, a solution is a list of acts that affect the
||| requirements of the problem.
|||
||| The list is the crowbar attached to the mighty hammer I am using
||| to make this work.
module Sif.Pattern

import public GRL
import public Sif.Problem

||| Within the 'Solution' EDSL there are several types of element.
data STy = ACT | ACTELEM | ACTION | ACTIONELEM | PROPERTY | SPEC

using (m : GModel MODEL, p : Problem m PSPEC)
  mutual

    ||| A design pattern is indexed over a problem specification, a
    ||| corresponding GRL model and a type.
    data Pattern : Problem m PSPEC -> GModel ty -> STy -> Type where
      ||| An act is an aspect of the solution that is used to address requirements.
      Act : (name : Maybe String)
          -> (e : EvalVal)
          -> Pattern p (Task name e) ACT

      ||| Acts can have sub requirements.
      |||
      ||| @rty The type of breakdown.
      ||| @x   The requirement being divided.
      ||| @ys  The requirements being linked to.
      HasSubActs : (x : Pattern p a ACT)
                 -> (rty : DTy)
                 -> (ys : Acts gys p)
                 -> Pattern p (genDComp rty a gys) ACTION

      ||| Acts will act upon requirements from the problem.
      |||
      ||| @a The act that provides the effect.
      ||| @c The value of the effect.
      ||| @f The requirement that is being affected.
      ActsOn : (a : Pattern p x ACT)
             -> (c : Contrib)
             -> (f : Problem g FORCE)
--             -> {auto prf : usesForce f p = Yes prf'}
             -> Pattern p (Impacts c x g) ACTION

      ||| Use of acts will affect other acts.
      |||
      ||| @a The act that provides the effect.
      ||| @c The value of the effect.
      ||| @b The act that is being affected.
      EffectsOn : (a : Pattern p x ACT)
             -> (c : Contrib)
             -> (b : Pattern p y ACT)
             -> Pattern p (Effects c x y) ACTION

      ||| Properties are aspects of the solution that contain acts and their actions.
      |||
      ||| Properties act as groups for 'acts', and the list of actions
      ||| can refer to acts from other properties.
      |||
      ||| @name The name of the property.
      ||| @as The list of acts associated with the property.
      ||| @es The list of actions associated with the property.
      Property : (name : String)
               -> (as : Acts gas p)
               -> (es : Actions ges p)
               -> Pattern p (GRLSpec gas ges) PROPERTY

      ||| Construct a pattern.
      |||
      ||| @title The model may have titles.
      ||| @ps    The list of properties associated with the solution.
      MkPattern : (title : Maybe String)
                -> (ps : Props gps p)
                -> Pattern p (foldGRLS m gps) SPEC

  -- --------------------------------------------------------- [ Helpers Special ]
    namespace Acts
      data Acts : List (GModel ELEM) -> Problem m PSPEC -> Type where
        Nil : Acts Nil p
        (::) : Pattern p e ACT -> Acts es p -> Acts (e::es) p

    namespace Actions
      data Actions : List (GModel LINK) -> Problem m PSPEC -> Type where
        Nil : Actions Nil p
        (::) : Pattern p e ACTION -> Actions es p -> Actions (e::es) p

    namespace Props
      data Props : List (GModel MODEL) -> Problem m PSPEC -> Type where
        Nil : Props Nil p
        (::) : Pattern p e PROPERTY -> Props es p -> Props (e::es) p

  -- --------------------------------------------------------------------- [ EOF ]
