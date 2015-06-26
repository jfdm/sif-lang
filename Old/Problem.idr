||| Representing Problems in a design pattern.
|||
||| Intuitively, a problem is a list of functional and non-functional
||| requirements, together with possible links between each
||| requirement.
module Sif.Problem

import public Data.Sigma.DList

import public GRL.Lang.GLang

import public Sif.Requirements

%access public
%default total


-- ------------------------------------------------------------- [ Problem ADT ]

mkFunctional : (s : String) -> (e : GLang ELEM ** Req e FuncTy)
mkFunctional s = (_ ** mkFunctional s GOALty Nothing)

mkUsability : (s : String) -> (e : GLang ELEM ** Req e UsabTy)
mkUsability s = (_ ** mkUsability s GOALty Nothing)

mkReliability : (s : String) -> (e : GLang ELEM ** Req e ReliTy)
mkReliability s = (_ ** mkReliability s GOALty Nothing)

mkPerformance : (s : String) -> (e : GLang ELEM ** Req e PerfTy)
mkPerformance s = (_ ** mkPerformance s GOALty Nothing)

mkSupportability : (s : String) -> (e : GLang ELEM ** Req e SuppTy)
mkSupportability s = (_ ** mkSupportability s GOALty Nothing)


||| Construct a problem.
data Problem : GModel -> Type where
  MkProblem : (desc : String)
            -> (fs  : DList (GLang ELEM)   (\x => Req x xty) es)
            -> (sfs : DList (GLang STRUCT) (SubReqLink)      ss)
            -> (ifs : DList (GLang INTENT) (IntentLink)      is)
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
