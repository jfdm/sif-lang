||| Representing Design Patterns..
|||
||| Intuitively, a solution is a list of acts that affect the
||| requirements of the problem.
|||
||| The list is the crowbar attached to the mighty hammer I am using
||| to make this work.
module Sif.Pattern.Solution

import Data.Sigma.DList

import public GRL.Lang.GLang
import public Sif.Pattern.Problem

-- ----------------------------------------------------------------- [ Actions ]

data Action : GLang ELEM -> Type where
  MkAction : (desc : String)
          -> (sval : Maybe SValue)
          -> Action (MkTask desc sval)

instance Show (Action x) where
  show (MkAction d s) = "[Action " ++ show d ++ " " ++ show s ++ "]"

eqAction : Action x -> Action y -> Bool
eqAction (MkAction xd xs) (MkAction yd ys) = xd == yd && xs == ys

instance Eq (Action x) where
  (==) = eqAction

-- ------------------------------------------------------------- [ Sub Actions ]
||| State that the action can be broken down into other actions
data SubActLink : GLang STRUCT -> Type where
  MkAndALink : Action x -> DList (GLang ELEM) (Action) ys -> SubActLink (x &= ys)
  MkIorALink : Action x -> DList (GLang ELEM) (Action) ys -> SubActLink (x |= ys)
  MkXorALink : Action x -> DList (GLang ELEM) (Action) ys -> SubActLink (x X= ys)

instance Show (SubActLink s) where
  show (MkAndALink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkIorALink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkXorALink a bs) = "[" ++ show a ++ showDList show bs ++ "]"

eqSALink : SubActLink x -> SubActLink y -> Bool
eqSALink {x} {y} _ _ = eqGLang x y

-- ------------------------------------------------------------ [ Intent Links ]
data Intention : Problem m  -> Type where
    ||| Actions will act upon requirements from the problem.
    ActsOn : (a : Action x)
          -> (c : Contrib)
          -> (f : Req y rty)
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


-- -------------------------------------------------------------------- [ Misc ]
getActionName : Pattern p e ACTION -> Maybe String
getActionName (Action name _) = name

findAction : String
           -> Actions p es
           -> Maybe (e : GModel ELEM ** Pattern p e ACTION)
findAction _ Nil     = Nothing
findAction n (x::xs) = case (getActionName x) of
  (Just m) => if n == m then Just (_ ** x) else findAction n xs
  Nothing  => findAction n xs

-- --------------------------------------------------------------------- [ EOF ]
