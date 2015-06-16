||| Representing Design Patterns..
|||
||| Intuitively, a solution is a list of acts that affect the
||| requirements of the problem.
|||
||| The list is the crowbar attached to the mighty hammer I am using
||| to make this work.
module Sif.Pattern.Solution

import Data.SigmaList

import public GRL.Model
import public GRL.Utils
import public Sif.Pattern.Problem

||| Within the 'Solution' EDSL there are several types of element.
data STy = ACTION | RELATION | PROPERTY | SPEC | EMPTY

using (m : GModel MODEL, p : Problem m PSPEC)
  ||| A design pattern is indexed over a problem specification, a
  ||| corresponding GRL model and a type.
  data Pattern : Problem m PSPEC -> GModel ty -> STy -> Type where
    Empty : Pattern p Empty EMPTY

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
--           -> {auto prf : usesForce f p = Yes prf'}
           -> Pattern p (Impacts c x g) RELATION

    ||| Use of acts will affect other acts.
    SideEffect : (a : Pattern p x ACTION)
              -> (c : Contrib)
              -> (b : Pattern p y ACTION)
              -> Pattern p (Effects c x y) RELATION

    ||| Properties are aspects of a solution that will affect
    ||| several forces in the problem.
    Property : (name : Maybe String)
            -> (actions : SigmaList (GModel ELEM) (\x => Pattern p x ACTION) gas)
            -> (links   : SigmaList (GModel LINK) (\x => Pattern p x RELATION) ges)
            -> Pattern p (GRLSpec gas ges) PROPERTY

    ||| Construct a pattern.
    MkPattern : (title : Maybe String)
              -> (p : Problem m PSPEC)
              -> (props : SigmaList (GModel MODEL) (\x => Pattern p x PROPERTY) ps)
              -> Pattern p (foldGRLS m ps) SPEC

-- ------------------------------------------------------------ [ Type Aliases ]
Actions : Problem {ty=MODEL} m PSPEC -> List (GModel ELEM) -> Type
Actions p es = SigmaList (GModel ELEM) (\x => Pattern p x ACTION) es

Relations : Problem {ty=MODEL} m PSPEC -> List (GModel LINK) -> Type
Relations p es = SigmaList (GModel LINK) (\x => Pattern p x RELATION) es

Properties : Problem {ty=MODEL} m PSPEC -> List (GModel MODEL) -> Type
Properties p es = SigmaList (GModel MODEL) (\x => Pattern p x PROPERTY) es

getActionName : Pattern p e ACTION -> Maybe String
getActionName (Action name _) = name

findAction : String
           -> Actions p es
           -> Maybe (e : GModel ELEM ** Pattern p e ACTION)
findAction _ Nil     = Nothing
findAction n (x::xs) = case (getActionName x) of
  (Just m) => if n == m then Just (_ ** x) else findAction n xs
  Nothing  => findAction n xs

-- -------------------------------------------------------------------- [ Show ]
showPattern : Pattern p e ty -> String
showPattern (Action n eval)       = unwords ["[Action ", show n, show eval, "]\n"]
showPattern (HasSubAction x ty y) = unwords ["[HasSubAction", showPattern x, show ty, showPattern y, "]\n"]

showPattern (ActsOn a c f) = unwords ["[ActsOn", showPattern a, show c, show f, "]\n"]
showPattern (SideEffect a c b) = unwords ["[SideEffect", showPattern a, show c, showPattern b, "]\n"]
showPattern (Property n as ls) = unwords ["[Property", show n, showSigmaList showPattern as, showSigmaList showPattern ls, "]\n"]
showPattern (MkPattern t p ps) = unwords ["[Pattern", show t, show p, showSigmaList showPattern ps, "]\n"]

instance Show (Pattern p e ty) where
  show = showPattern


instance Semigroup (Pattern p e ty) where
  (<+>) Empty Empty = Empty
  (<+>)

instance Monoid (Pattern p e ty) where
  neutral = Empty
{-

instance Semigroup Doc where
  (<+>) = beside

||| Note that the neutral element is not a left and right unit with
||| respect to propositional equality of the underlying Doc syntax
||| tree, but rather with respect to the equality of the result of
||| rendering. So it's "morally" a `Monoid`.
instance Monoid Doc where
  neutral = empty

-}
-- --------------------------------------------------------------------- [ EOF ]
