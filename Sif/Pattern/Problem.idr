||| Representing Problems in a design pattern.
|||
||| Intuitively, a problem is a list of functional and non-functional
||| requirements, together with possible links between each
||| requirement.
module Sif.Pattern.Problem

import Decidable.Equality
import GRL

%access public

||| Within the Problem EDSL there are three types of element.
data PTy = FORCE | FLINK | PSPEC
data DTy = ANDTy | XORTy | IORTy

instance Show DTy where
  show ANDTy = "&&"
  show XORTy = "XOR"
  show IORTy = "||"

genDComp : DTy -> GModel ELEM -> GModel ELEM -> GModel LINK
genDComp ty x y with (ty)
  | ANDTy = AND x [y]
  | XORTy = XOR x [y]
  | IORTy = IOR x [y]

mutual

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
              -> (es : Reqs res)
              -> (ls : RLinks rls)
              -> Problem (GRLSpec res rls) PSPEC

-- ----------------------------------------------------------------- [ Helpers ]
  namespace Reqs
    data Reqs : List (GModel ELEM) -> Type where
      Nil : Reqs Nil
      (::) : Problem e FORCE -> Reqs es -> Reqs (e::es)

  namespace RLinks
    data RLinks : List (GModel LINK) -> Type where
      Nil : RLinks Nil
      (::) : Problem e FLINK -> RLinks es -> RLinks (e::es)

mutual
  showReqs : Reqs es -> List String
  showReqs Nil     = [""]
  showReqs (x::xs) = show x :: showReqs xs

  instance Show (Reqs es) where
    show xs = "[" ++ concat (intersperse "," (showReqs xs)) ++ "]"

  showRLinks : RLinks es -> List String
  showRLinks Nil     = [""]
  showRLinks (x::xs) = show x :: showRLinks xs

  instance Show (RLinks es) where
    show xs = "[" ++ concat (intersperse "," (showRLinks xs)) ++ "]"


  instance Show (Problem x ty) where
    show (MkProblem d es ls) = unwords ["[Problem ", show d, show es, show ls,"]\n"]
    show (EffectsUpon a c b) = unwords ["[Effect", show a, show c, show b, "]\n"]
    show (ImpactsUpon a c b) = unwords ["[Impact", show a, show c, show b, "]\n"]
    show (HasSubReq dty a b) = unwords ["[SubReq", show dty, show a, show b, "]\n"]
    show (Functional n)      = unwords ["[Functional", show n, "]\n"]
    show (Usability n)       = unwords ["[Usability", show n, "]\n"]
    show (Reliability n)     = unwords ["[Reliability", show n, "]\n"]
    show (Performance n)     = unwords ["[Performance", show n, "]\n"]
    show (Supportability n)  = unwords ["[Supportability", show n, "]\n"]

-- ------------------------------------------------------------- [ Eq Instance ]
mutual
  forceEq : Problem a FORCE -> Problem b FORCE -> Bool
  forceEq (Functional x)         (Functional y)         = x == y
  forceEq (Usability x)          (Usability y)          = x == y
  forceEq (Reliability x)        (Reliability y)        = x == y
  forceEq (Performance x)        (Performance y)        = x == y
  forceEq (Supportability x)     (Supportability y)     = x == y

  reqEq : Reqs xs -> Reqs ys -> Bool
  reqEq Nil     Nil     = True
  reqEq (x::xs) (y::ys) = if forceEq x y then reqEq xs ys else False
  reqEq _       _       = False

-- TODO do properly
problemEq : Problem a PSPEC -> Problem b PSPEC -> Bool
problemEq (MkProblem x xs xxs) (MkProblem y ys yys) = x == y

-- ----------------------------------------------- [ Force Deciadable Equality ]
-- @TODO make deeply decidable...
forceDecEq : (x : Problem a FORCE) -> (y : Problem b FORCE) -> Dec (x = y)
forceDecEq x y = if forceEq x y
                   then Yes forcePrimEq
                   else No forcePrimNotEq
    where
      forcePrimEq : x = y
      forcePrimEq = believe_me (Refl {x})
      postulate forcePrimNotEq : x = y -> Void

data ExistsForce : (f : Problem m FORCE) -> Reqs ms -> Type where
  Here  : ExistsForce f (f::fs)
  There : ExistsForce f fs -> ExistsForce f (f'::fs)

instance Uninhabited (ExistsForce x Nil) where
  uninhabited Here      impossible
  uninhabited (There p) impossible


data ContainsForce : Problem g FORCE -> Problem m PSPEC -> Type where
  UsesForce : ExistsForce f fs -> ContainsForce f p

-- @TODO Get rid of believe_me
forceIsUsed : (f : Problem g FORCE) -> (fs : Reqs res) -> Dec (ExistsForce f fs)
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


-- usesForce : (f : Problem g FORCE)
--          -> (p : Problem m PSPEC)
--          -> Dec (ContainsForce f p)
-- usesForce f (MkProblem d es ls) with (forceIsUsed f es)
--    | (Yes prf)   = Yes (UsesForce prf)
--    | (No absurd) = No absurd

{-
-- [ Proof that same forces that are equal ]

funcInj : (Functional x) = (Functional y) -> x = y
funcInj Refl = Refl

usabInj : (Usability x) = (Usability y) -> x = y
usabInj Refl = Refl

reliaInj : (Reliability x) = (Reliability y) -> x = y
reliaInj Refl = Refl

perfInj : (Performance x) = (Performance y) -> x = y
perfInj Refl = Refl

suppInj : (Supportability x) = (Supportability y) -> x = y
suppInj Refl = Refl

-- [ Proof that different forces are not equal. ]

funcNotUsab : (Functional x) = (Usability y) -> Void
funcNotUsab Refl impossible

funcNotRelia : (Functional x) = (Reliability y) -> Void
funcNotRelia Refl impossible

funcNotPerf : (Functional x) = (Performance y) -> Void
funcNotPerf Refl impossible

funcNotSupp : (Functional x) = (Supportability y) -> Void
funcNotSupp Refl impossible

usabNotRelia : (Usability x) = (Reliability y) -> Void
usabNotRelia Refl impossible

usabNotPerf : (Usability x) = (Performance y) -> Void
usabNotPerf Refl impossible

usabNotSupp : (Usability x) = (Supportability y) -> Void
usabNotSupp Refl impossible

reliaNotPerf : (Reliability x) = (Performance y) -> Void
reliaNotPerf Refl impossible

reliaNotSupp : (Reliability x) = (Supportability y) -> Void
reliaNotSupp Refl impossible

perfNotSupp : (Performance x) = (Supportability y) -> Void
perfNotSupp Refl impossible

-- [ Decidable Eq Instance ]

forceEq' : (x : Problem a FORCE)
        -> (y : Problem b FORCE)
        -> (a=b)
        -> x = y
forceEq' (Functional x) (Usability y)      impossible
forceEq' (Functional x) (Reliability y)    Refl = funcNotRelia
forceEq' (Functional x) (Performance y)    Refl = funcNotPerf
forceEq' (Functional x) (Supportability y) Refl = funcNotSupp

forceEq' (Usability x)  (Functional y)     = (neqEqSym funcNotUsab)
forceEq' (Usability x)  (Reliability y)    = usabNotRelia
forceEq' (Usability x)  (Performance y)    = usabNotPerf
forceEq' (Usability x)  (Supportability y) = usabNotSupp

forceEq' (Reliability x) (Functional y)     = (neqEqSym funcNotRelia)
forceEq' (Reliability x) (Usability y)      = (neqEqSym usabNotRelia)
forceEq' (Reliability x) (Performance y)    = reliaNotPerf
forceEq' (Reliability x) (Supportability y) = reliaNotSupp

forceEq' (Performance x) (Functional y)     = (neqEqSym funcNotPerf)
forceEq' (Performance x) (Usability y)      = (neqEqSym usabNotPerf)
forceEq' (Performance x) (Reliability y)    = (neqEqSym reliaNotPerf)
forceEq' (Performance x) (Supportability y) = perfNotSupp

forceEq' (Supportability x) (Functional y)  = (neqEqSym funcNotSupp)
forceEq' (Supportability x) (Usability y)   = (neqEqSym usabNotSupp)
forceEq' (Supportability x) (Reliability y) = (neqEqSym reliaNotSupp)
forceEq' (Supportability x) (Performance y) = (neqEqSym perfNotSupp)



    --   forceEq (Functional x) (Functional y) with (forceEq x y)
    -- forceEq (Functional x) (Functional y) | (Yes Refl) = Yes Refl
    -- forceEq (Functional x) (Functional y) | (No prf)   = No (\p => prf (funcInj p))


forceEq : (x : Problem a FORCE)
        -> (y : Problem b FORCE)
        -> (a=b)
        -> x = y
forceEq f1 f2 prf = forceEq' f1 f2 Refl prf

{-
instance Eq (Problem m FORCE) where
  (==) = forceEq

instance DecEq (Problem m FORCE) where
  decEq = forceDecEq

-}
-- data ContainsForce : Problem m FORCE -> Problem g PSPEC -> Type where
--   IsThere :

-}

getReqName : Problem e FORCE -> Maybe String
getReqName (Functional name) = name
getReqName (Usability name) = name
getReqName (Reliability name) = name
getReqName (Performance name) = name
getReqName (Supportability name) = name

findReq : String
       -> Reqs rs
       -> Maybe (e : GModel ELEM ** Problem e FORCE)
findReq _ Nil     = Nothing
findReq n (x::xs) = case (getReqName x) of
  (Just m) => if n == m then Just (_ ** x) else findReq n xs
  Nothing  => findReq n xs

-- --------------------------------------------------------------------- [ EOF ]
