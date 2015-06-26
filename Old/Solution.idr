||| Representing Design Patterns..
|||
||| Intuitively, a solution is a list of acts that affect the
||| requirements of the problem.
|||
||| The list is the crowbar attached to the mighty hammer I am using
||| to make this work.
module Sif.Solution

import Data.Sigma.DList

import public GRL.Lang.GLang
import public Sif.Problem

%access public
%default total

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

getActionName : Action e -> String
getActionName (MkAction d _) = d

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
eqSALink {x} {y} _ _ = x == y

data Intention : GLang INTENT -> Type where
  AffectReq : (c : CValue)
           -> (a : Action x)
           -> (r : Req y rty)
           -> Intention (x ==> y | c)
  AffectAct : (c : CValue)
           -> (a : Action x)
           -> (b : Action y)
           -> Intention (x ==> y | c)

data Property : DList GTy GLang gs -> Type where
  MkProperty : (desc : String)
            -> (as  : DList (GLang ELEM)   (Action)     es)
            -> (sas : DList (GLang STRUCT) (SubActLink) ss)
            -> (ias : DList (GLang INTENT) (Intention)  is)
            -> Property ((getProof $ fromList es) ++
                         (getProof $ fromList ss) ++
                         (getProof $ fromList is))

data Properties : DList GTy GLang gs -> Type where
  Nil  : Properties Nil
  (::) : Property gs -> Properties ggs -> Properties (gs ++ ggs)

data Solution : DList GTy GLang gs -> Type where
  MkSolution : (title : String)
            -> (ps : Properties ggs)
            -> Solution ggs

-- -------------------------------------------------------------------- [ Misc ]
{-
getActionName : Pattern p e ACTION -> Maybe String
getActionName (Action name _) = name

findAction : String
           -> Actions p es
           -> Maybe (e : GModel ELEM ** Pattern p e ACTION)
findAction _ Nil     = Nothing
findAction n (x::xs) = case (getActionName x) of
  (Just m) => if n == m then Just (_ ** x) else findAction n xs
  Nothing  => findAction n xs
-}
-- --------------------------------------------------------------------- [ EOF ]
