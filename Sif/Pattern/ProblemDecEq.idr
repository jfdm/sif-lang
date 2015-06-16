||| Representing Problems in a design pattern.
|||
||| Intuitively, a problem is a list of functional and non-functional
||| requirements, together with possible links between each
||| requirement.
module Sif.Pattern.Problem

import Decidable.Equality


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
