||| Representing Problems in a design pattern.
|||
||| Intuitively, a problem is a list of functional and non-functional
||| requirements, together with possible links between each
||| requirement.
module Sif.Pattern.Problem

import public Data.Sigma.DList

import GRL.Lang.GLang

%access public
%default total

-- ------------------------------------------------------- [ Requirement Types ]

||| Within the Problem EDSL there are five types of force.
data FTy = FuncTy | UsabTy | ReliTy | PerfTy | SuppTy

instance Show FTy where
  show FuncTy  = "FuncTy"
  show UsabTy  = "UsabTy"
  show ReliTy  = "ReliTy"
  show PerfTy  = "PerfTy"
  show SuppTy  = "SuppTy"

instance Eq FTy where
  (==) FuncTy FuncTy = True
  (==) UsabTy UsabTy = True
  (==) ReliTy ReliTy = True
  (==) PerfTy PerfTy = True
  (==) SuppTy SuppTy = True
  (==) _      _      = False

-- --------------------------------------------------------- [ Requirement ADT ]
private
mkG : String -> GLang ELEM
mkG s = MkGoal s Nothing

data Req : GLang ELEM -> FTy -> Type where
  ||| Requirements relating to functionality and security.
  |||
  ||| Functionality - Capability (Size & Generality of Feature Set),
  ||| Reusability (Compatibility, Interoperability, Portability),
  ||| Security (Safety & Exploitability)
  MkFunctional : (desc : String) -> Req (mkG desc) FuncTy

  ||| Requirements relating to usability metters.
  |||
  ||| Usability (UX) - Human Factors, Aesthetics, Consistency,
  ||| Documentation, Responsiveness
  MkUsability : (desc : String) -> Req (mkG desc) UsabTy

  ||| Requirements relating to reliability matters.
  |||
  ||| Reliability - Availability (Failure Frequency
  ||| (Robustness/Durability/Resilience), Failure Extent &
  ||| Time-Length (Recoverability/Survivability)), Predictability
  ||| (Stability), Accuracy (Frequency/Severity of Error)
  MkReliability : (desc : String) -> Req (mkG desc) ReliTy

  ||| Requirements relating to perf matters.
  |||
  ||| For example: Performance - Speed, Efficiency, Resource
  ||| Consumption (power, ram, cache, etc.), Throughput, Capacity,
  ||| Scalability
  MkPerformance : (desc : String) -> Req (mkG desc) PerfTy

  ||| Requirements relating to the maintenance of the problem.
  |||
  ||| For example: Supportability (Serviceability, Maintainability,
  ||| Sustainability, Repair Speed) - Testability, Flexibility
  ||| (Modifiability, Configurability, Adaptability, Extensibility,
  ||| Modularity), Installability, Localizability
  MkSupportability : (desc : String) -> Req (mkG desc) SuppTy

instance Show (Req e ty) where
  show (MkFunctional s) = "[Functional " ++ show s ++ "]"
  show (MkUsability s) = "[Usability " ++ show s ++ "]"
  show (MkReliability s) = "[Reliability " ++ show s ++ "]"
  show (MkPerformance s) = "[Performance " ++ show s ++ "]"
  show (MkSupportability s) = "[Supportability " ++ show s ++ "]"

private
eqRequire : Req ex xty -> Req ey yty -> Bool
eqRequire (MkFunctional x)     (MkFunctional y)     = x == y
eqRequire (MkUsability x)      (MkUsability y)      = x == y
eqRequire (MkReliability x)    (MkReliability y)    = x == y
eqRequire (MkPerformance x)    (MkPerformance y)    = x == y
eqRequire (MkSupportability x) (MkSupportability y) = x == y
eqRequire _                     _                    = False

instance Eq (Req e ty) where
  (==) = eqRequire

-- ----------------------------------------------------- [ Sub Requirement ADT ]

||| State that the requirement can be broken down into other
||| requirements.
|||
data SubReqLink : GLang STRUCT -> Type where
  MkAndLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x &= ys)
  MkIorLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x |= ys)
  MkXorLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x X= ys)

instance Show (SubReqLink s) where
  show (MkAndLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkIorLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkXorLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"

eqSRLink : SubReqLink x -> SubReqLink y -> Bool
eqSRLink {x} {y} _ _ = x == y

-- @TODO Add Eq instance
-- eqSRLink : SubReqLink x -> SubReqLink y -> Bool
-- eqSRLink (MkAndLink a as)  (MkAndLink b bs) = b == a && eqDList (\x,y => eqRequire x y) as bs
-- eqSRLink (MkIorLink a as)  (MkIorLink b bs) = b == a && eqDList (\x,y => eqRequire x y) as bs
-- eqSRLink (MkXorLink a as)  (MkXorLink b bs) = b == a && eqDList (\x,y => eqRequire x y) as bs
-- eqSRLink _                 _                = False

-- --------------------------------------------------------- [ IntentLink ADTs ]

||| State the impact or effect that a force has on another force.
data IntentLink : GLang INTENT -> Type where
  MkFImpact : (c : CValue) -> Req x xty -> Req y yty -> IntentLink (x ==> y | c)
  MkFEffect : (c : CValue) -> Req x xty -> Req y yty -> IntentLink (x ~~> y | c)

instance Show (IntentLink i) where
  show (MkFImpact c a b) = "[Req Impacts " ++ show a ++ " " ++ show b ++ "]"
  show (MkFEffect c a b) = "[Req Effects " ++ show a ++ " " ++ show b ++ "]"


-- @TODO Add Eq Instance

eqIntentLink : IntentLink x -> IntentLink y -> Bool
eqIntentLink (MkFImpact xc xa xb) (MkFImpact yc ya yb) = xc == yc && eqRequire xa ya && eqRequire xb yb
eqIntentLink (MkFEffect xc xa xb) (MkFEffect yc ya yb) = xc == yc && eqRequire xa ya && eqRequire xb yb
eqIntentLink _              _                          = False

-- ------------------------------------------------------------- [ Problem ADT ]
||| Construct a problem.
data Problem : GModel -> Type where
  MkProblem : (desc : String)
            -> (fs  : DList (GLang ELEM)   (\x => Req x xty) es)
            -> (sfs : DList (GLang STRUCT) (SubReqLink)      ss)
            -> (ifs : DList (GLang INTENT) (IntentLink)        is)
            -> Problem (insertMany is $ insertMany ss $ insertMany es emptyModel)

instance Show (Problem m) where
  show (MkProblem d fs sfs ifs) = with List
      unwords ["[Problem", show d,
                           showDList show fs,
                           showDList show sfs,
                           showDList show ifs,"]"]

eqProblem : Problem a -> Problem b -> Bool
eqProblem (MkProblem x xfs xsfs xifs) (MkProblem y yfs ysfs yifs) =
  x == y &&
  showDList show xfs == showDList show yfs &&
  eqDList eqSRLink  xsfs ysfs &&
  eqDList eqIntentLink xifs yifs
eqProblem _ _ = False

getModelProblem : Problem m -> GModel
getModelProblem {m} _ = m
-- --------------------------------------------------------------------- [ EOF ]
