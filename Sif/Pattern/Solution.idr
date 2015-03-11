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
                -> Pattern p (Effects c x y) RELATION

      ||| Properties are aspects of a solution that will affect
      ||| several forces in the problem.
      Property : (name : Maybe String)
              -> (actions : Actions p as)
              -> (links : Relations p rs)
              -> Pattern p (GRLSpec gas ges) PROPERTY

      ||| Construct a pattern.
      MkPattern : (title : Maybe String)
                -> (p : Problem m PSPEC)
                -> (props : Properties p ps)
                -> Pattern p (foldGRLS m ps) SPEC

  -- --------------------------------------------------------- [ Helpers Special ]
    namespace Actions
      data Actions : (p : Problem m PSPEC) -> List (GModel ELEM) -> Type where
        Nil : Actions p Nil
        (::) : Pattern p e ACTION -> Actions p es -> Actions p (e::es)

    namespace Relations
      data Relations : Problem m PSPEC -> List (GModel LINK) -> Type where
        Nil : Relations p Nil
        (::) : Pattern p e RELATION -> Relations p es -> Relations p (e::es)

    namespace Properties
      data Properties :  Problem m PSPEC -> List (GModel MODEL) -> Type where
        Nil  : Properties p Nil
        (::) : Pattern p e PROPERTY -> Properties p es -> Properties p (e::es)


getActionName : Pattern p e ACTION -> Maybe String
getActionName (Action name _) = name

findAction : String
           -> Actions p es
           -> Maybe (e : GModel ELEM ** Pattern p e ACTION)
findAction _ Nil     = Nothing
findAction n (x::xs) = case (getActionName x) of
  (Just m) => if n == m then Just (_ ** x) else findAction n xs
  Nothing  => findAction n xs

relationAppend : Relations p as -> Relations p bs -> Relations p (as ++ bs)
relationAppend Nil     ys = ys
relationAppend (x::xs) ys = Relations.(::) x (relationAppend xs ys)

mutual
  showActions : Actions p es -> List String
  showActions Nil     = [""]
  showActions (x::xs) = show x :: showActions xs

  instance Show (Actions p es) where
    show xs = "[" ++ concat (intersperse "," (showActions xs)) ++ "]"

  showRelations : Relations p es -> List String
  showRelations Nil     = [""]
  showRelations (x::xs) = show x :: showRelations xs

  instance Show (Relations p es) where
    show xs = "[" ++ concat (intersperse "," (showRelations xs)) ++ "]"

  showProperties : Properties p es -> List String
  showProperties Nil     = [""]
  showProperties (x::xs) = show x :: showProperties xs

  instance Show (Properties p es) where
    show xs = "[" ++ concat (intersperse "," (showProperties xs)) ++ "]"


  showPattern : Pattern p e ty -> String
  showPattern (Action n eval)       = unwords ["[Action ", show n, show eval, "]\n"]
  showPattern (HasSubAction x ty y) = unwords ["[HasSubAction", show x, show ty, show y, "]\n"]

  showPattern (ActsOn a c f)     = unwords ["[ActsOn", show a, show c, show f, "]\n"]
  showPattern (SideEffect a c b) = unwords ["[SideEffect", show a, show c, show b, "]\n"]
  showPattern (Property n as ls) = unwords ["[Property", show n, show as, show ls, "]\n"]
  showPattern (MkPattern t p ps) = unwords ["[Pattern", show t, show p, show ps, "]\n"]

  instance Show (Pattern p e ty) where
    show = showPattern

-- --------------------------------------------------------------------- [ EOF ]
