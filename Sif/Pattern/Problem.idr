||| Representing Problems in a design pattern.
|||
||| Intuitively, a problem is a list of functional and non-functional
||| requirements, together with possible links between each
||| requirement.
module Sif.Pattern.Problem

import public Data.SigmaList

import GRL.Model

%access public

||| Within the Problem EDSL there are three types of element.
data PTy = FORCE | FLINK | PSPEC
data DTy = ANDTy | XORTy | IORTy

instance Show DTy where
  show ANDTy = "&&"
  show XORTy = "XOR"
  show IORTy = "||"

instance Eq DTy where
  (==) ANDTy ANDTy = True
  (==) XORTy XORTy = True
  (==) IORTy IORTy = True
  (==) _     _     = False

genDComp : DTy -> GModel ELEM -> GModel ELEM -> GModel LINK
genDComp ty x y with (ty)
  | ANDTy = AND x [y]
  | XORTy = XOR x [y]
  | IORTy = IOR x [y]

||| A model for a problem in the context of a design patterns.
||| The requirement types are taken from the FURPS methodology.
data Problem : GModel ty -> PTy -> Type where

  ||| Requirements relating to functionality and security.
  |||
  ||| Functionality - Capability (Size & Generality of Feature Set),
  ||| Reusability (Compatibility, Interoperability, Portability),
  ||| Security (Safety & Exploitability)
  Functional : (name : Maybe String)
             -> Problem (Goal name UNKNOWN) FORCE

  ||| Requirements relating to usability metters.
  |||
  ||| Usability (UX) - Human Factors, Aesthetics, Consistency,
  ||| Documentation, Responsiveness
  Usability : (name : Maybe String)
            -> Problem (Goal name UNKNOWN) FORCE

  ||| Requirements relating to reliability matters.
  |||
  ||| Reliability - Availability (Failure Frequency
  ||| (Robustness/Durability/Resilience), Failure Extent &
  ||| Time-Length (Recoverability/Survivability)), Predictability
  ||| (Stability), Accuracy (Frequency/Severity of Error)
  Reliability : (name : Maybe String)
              -> Problem (Goal name UNKNOWN) FORCE

  ||| Requirements relating to perf matters.
  |||
  ||| For example: Performance - Speed, Efficiency, Resource
  ||| Consumption (power, ram, cache, etc.), Throughput, Capacity,
  ||| Scalability
  Performance : (name : Maybe String)
              -> Problem (Goal name UNKNOWN) FORCE

  ||| Requirements relating to the maintenance of the problem.
  |||
  ||| For example: Supportability (Serviceability, Maintainability,
  ||| Sustainability, Repair Speed) - Testability, Flexibility
  ||| (Modifiability, Configurability, Adaptability, Extensibility,
  ||| Modularity), Installability, Localizability
  Supportability : (name : Maybe String)
                 -> Problem (Goal name UNKNOWN) FORCE

  ||| State that the requirement can be broken down into other
  ||| requirements. TODO Fix
  |||
  ||| @rty The type of breakdown.
  ||| @a   The requirement being divided.
  ||| @b   The requirements being linked to.
  HasSubReq : (rty : DTy)
            -> (a : Problem x FORCE)
            -> (b : Problem y FORCE)
            -> Problem (genDComp rty x y) FLINK

  ||| State that requirement `a` has a direct impact upon
  ||| requirement `b` by `c` amount.
  |||
  ||| @a The requirement that provides the impact.
  ||| @c The value of the impact.
  ||| @b The requirement that will be impacted.
  ImpactsUpon : (a : Problem x FORCE)
              -> (c : Contrib)
              -> (b : Problem y FORCE)
              -> Problem (Impacts c x y) FLINK

  ||| State that requirement `a` affects requirement `b` by `c` contribution.
  |||
  ||| @a The requirement that provides the effect.
  ||| @c The value of the affect.
  ||| @b The requirement that will be affected.
  EffectsUpon : (a : Problem x FORCE)
              -> (c : Contrib)
              -> (b : Problem y FORCE)
              -> Problem (Effects c x y) FLINK

  ||| Construct a problem definition.
  |||
  ||| @desc The problem description.
  ||| @es   The list of problem requirements.
  ||| @ls   The list of links between requirements.
  MkProblem : (desc : Maybe String)
            -> (es : SigmaList (GModel ELEM) (\x => Problem x FORCE) res)
            -> (ls : SigmaList (GModel LINK) (\x => Problem x FLINK) rls)
            -> Problem (GRLSpec res rls) PSPEC

Reqs : List (GModel ELEM) -> Type
Reqs rs = SigmaList (GModel ELEM) (\x => Problem x FORCE) rs


-- -------------------------------------------------------------------- [ Show ]

showProblem : Problem x ty -> String
showProblem (MkProblem d es ls) = unwords ["[Problem ", show d, showSigmaList (showProblem) es, showSigmaList (showProblem) ls, "]\n"]
showProblem (EffectsUpon a c b) = unwords ["[Effect", showProblem a, show c, showProblem b, "]\n"]
showProblem (ImpactsUpon a c b) = unwords ["[Impact", showProblem a, show c, showProblem b, "]\n"]
showProblem (HasSubReq dty a b) = unwords ["[SubReq", show dty, showProblem a, showProblem b, "]\n"]
showProblem (Functional n)      = unwords ["[Functional", show n, "]\n"]
showProblem (Usability n)       = unwords ["[Usability", show n, "]\n"]
showProblem (Reliability n)     = unwords ["[Reliability", show n, "]\n"]
showProblem (Performance n)     = unwords ["[Performance", show n, "]\n"]
showProblem (Supportability n)  = unwords ["[Supportability", show n, "]\n"]

instance Show (Problem x ty) where
  show = showProblem

-- ------------------------------------------------------------- [ Eq Instance ]

eqProblem : Problem a aTy -> Problem b bTy -> Bool
eqProblem (Functional x)         (Functional y)         = x == y
eqProblem (Usability x)          (Usability y)          = x == y
eqProblem (Reliability x)        (Reliability y)        = x == y
eqProblem (Performance x)        (Performance y)        = x == y
eqProblem (Supportability x)     (Supportability y)     = x == y

eqProblem (EffectsUpon xa xc xb) (EffectsUpon ya yc yb) = eqProblem xa ya && xc == yc && eqProblem xb yb
eqProblem (ImpactsUpon xa xc xb) (ImpactsUpon ya yc yb) = eqProblem xa ya && xc == yc && eqProblem xb yb
eqProblem (HasSubReq xDty xa xb) (HasSubReq yDty ya yb) = eqProblem xa ya && xDty == yDty && eqProblem ya yb

eqProblem (MkProblem x xs xxs)   (MkProblem y ys yys) = x == y && eqSigmaList eqProblem xs ys && eqSigmaList eqProblem xxs yys


-- ----------------------------------------------- [ Force Deciadable Equality ]
-- @TODO make deeply decidable...
forceDecEq : (x : Problem a FORCE) -> (y : Problem b FORCE) -> Dec (x = y)
forceDecEq x y = if eqProblem x y
                   then Yes forcePrimEq
                   else No forcePrimNotEq
    where
      forcePrimEq : x = y
      forcePrimEq = believe_me (Refl {x})
      postulate forcePrimNotEq : x = y -> Void

data ExistsForce : (f : Problem m FORCE) -> SigmaList (GModel ELEM) (\x => Problem x FORCE) ms -> Type where
  Here  : ExistsForce f (f::fs)
  There : ExistsForce f fs -> ExistsForce f (f'::fs)

instance Uninhabited (ExistsForce x Nil) where
  uninhabited Here      impossible
  uninhabited (There p) impossible


data ContainsForce : Problem g FORCE -> Problem m PSPEC -> Type where
  UsesForce : ExistsForce f fs -> ContainsForce f p

-- @TODO Get rid of believe_me
forceIsUsed : (f : Problem g FORCE) -> (ls : SigmaList (GModel ELEM) (\x => Problem x FORCE) fs) -> Dec (ExistsForce f ls)
forceIsUsed x Nil        = No absurd
forceIsUsed x (y :: xs) with (forceDecEq x y)
  forceIsUsed x (x :: xs) | (Yes Refl) = Yes Here
  forceIsUsed x (y :: xs) | (No contra) with (forceIsUsed x xs)
    forceIsUsed x (y :: xs) | (No contra) | (Yes prf) = Yes (There prf)
    forceIsUsed x (y :: xs) | (No contra) | (No f)    = No (believe_me)

-- @TODO Get rid of believe_me
usesForce : (f : Problem g FORCE) -> (p : Problem m PSPEC) -> Dec (ContainsForce f p)
usesForce f p with (p)
  | (MkProblem d es ls) with (forceIsUsed f es)
    | (Yes prf)    = Yes (UsesForce prf)
    | (No  contra) = No (believe_me)

getReqName : Problem e FORCE -> Maybe String
getReqName (Functional name) = name
getReqName (Usability name) = name
getReqName (Reliability name) = name
getReqName (Performance name) = name
getReqName (Supportability name) = name

findReq : String
       -> SigmaList (GModel ELEM) (\x => Problem x FORCE) rs
       -> Maybe (e : GModel ELEM ** Problem e FORCE)
findReq _ Nil     = Nothing
findReq n (x::xs) = case (getReqName x) of
  (Just m) => if n == m then Just (_ ** x) else findReq n xs
  Nothing  => findReq n xs

-- --------------------------------------------------------------------- [ EOF ]
