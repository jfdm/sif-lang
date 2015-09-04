-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.API

import GRL.Lang.GLang
import Sif.Types
import Sif.Pattern.Model

-- ----------------------------------------------------------- [ Type Synonyms ]

FUNCTIONAL : (impl : SifTy -> Type) -> Type
FUNCTIONAL impl = SifExpr impl tyREQ

USABILITY :  (impl : SifTy -> Type) -> Type
USABILITY impl = SifExpr impl tyREQ

RELIABILITY :  (impl : SifTy -> Type) -> Type
RELIABILITY impl = SifExpr impl tyREQ

PERFORMANCE :  (impl : SifTy -> Type) -> Type
PERFORMANCE impl = SifExpr impl tyREQ

SUPPORTABILITY :  (impl : SifTy -> Type) -> Type
SUPPORTABILITY impl = SifExpr impl tyREQ

REQUIREMENT :  (impl : SifTy -> Type) -> Type
REQUIREMENT impl = SifExpr impl tyREQ

REQUIREMENTS :  (impl : SifTy -> Type) -> Type
REQUIREMENTS impl = List (REQUIREMENT impl)

PROBLEM :  (impl : SifTy -> Type) -> Type
PROBLEM impl = SifExpr impl tyPROBLEM

ADVANTAGE :  (impl : SifTy -> Type) -> Type
ADVANTAGE impl = SifExpr impl tyTRAIT

DISADVANTAGE :  (impl : SifTy -> Type) -> Type
DISADVANTAGE impl = SifExpr impl tyTRAIT

TRAIT :  (impl : SifTy -> Type) -> Type
TRAIT impl = SifExpr impl tyTRAIT

TRAITS :  (impl : SifTy -> Type) -> Type
TRAITS impl = List (TRAIT impl)

AFFECT :  (impl : SifTy -> Type) -> Type
AFFECT impl = SifExpr impl tyAFFECTS

AFFECTS :  (impl : SifTy -> Type) -> Type
AFFECTS impl = List (AFFECT impl)

PROPERTY :  (impl : SifTy -> Type) -> Type
PROPERTY impl = SifExpr impl tyPROPERTY

PROPERTIES :  (impl : SifTy -> Type) -> Type
PROPERTIES impl = List (PROPERTY impl)

SOLUTION :  (impl : SifTy -> Type) -> Type
SOLUTION impl = SifExpr impl tySOLUTION

PATTERN :  (impl : SifTy -> Type) -> Type
PATTERN impl = SifExpr impl tyPATTERN

-- ------------------------------------------------------------- [ Problem API ]

mkRequirement : SifBuilder i
             -> RTy
             -> String
             -> Maybe String
             -> REQUIREMENT i
mkRequirement impl ty s desc = (buildReq impl) ty s desc

mkFunctional : SifBuilder i
            -> String
            -> Maybe String
            -> FUNCTIONAL i
mkFunctional impl s desc = (buildReq impl) FUNC s desc

mkUsability : SifBuilder i
           -> String
           -> Maybe String
           -> USABILITY i
mkUsability impl s desc = (buildReq impl) USAB s desc

mkReliability : SifBuilder i
             -> String
             -> Maybe String
             -> RELIABILITY i
mkReliability impl s desc = (buildReq impl) RELI s desc

mkPerformance : SifBuilder i
             -> String
             -> Maybe String
             -> PERFORMANCE i
mkPerformance impl s desc = (buildReq impl) PERF s desc

mkSupportability : SifBuilder i
                -> String
                -> Maybe String
                -> SUPPORTABILITY i
mkSupportability impl s desc = (buildReq impl) SUPP s desc

mkProblem : SifBuilder i
         -> String
         -> Maybe String
         -> REQUIREMENTS i
         -> PROBLEM i
mkProblem impl s d rs = (buildProblem impl) s d rs

-- ------------------------------------------------------------ [ Solution API ]

mkAffect : SifBuilder i
        -> CValue
        -> REQUIREMENT i
        -> Maybe String
        -> AFFECT i
mkAffect impl c r d = (buildAffect impl) c r d

mkTrait : SifBuilder i
       -> TTy
       -> String
       -> Maybe String
       -> SValue
       -> AFFECTS i
       -> TRAIT i
mkTrait impl ty t d s rs = (buildTrait impl) ty t d s' rs
  where
    s' : SValue
    s' = case ty of {GEN => s; ADV => s; DIS => invertEval s}

mkAspect : SifBuilder i
       -> String
       -> Maybe String
       -> SValue
       -> AFFECTS i
       -> TRAIT i
mkAspect impl t d s rs = (buildTrait impl) GEN t d s rs

mkAdvantage : SifBuilder i
           -> String
           -> Maybe String
           -> SValue
           -> AFFECTS i
           -> ADVANTAGE i
mkAdvantage impl t d s rs = (buildTrait impl) ADV t d s rs

mkDisadvantage : SifBuilder i
              -> String
              -> Maybe String
              -> SValue
              -> AFFECTS i
              -> DISADVANTAGE i
mkDisadvantage impl t d s rs = (buildTrait impl) DIS t d (invertEval s) rs

mkProperty : SifBuilder i
          -> String
          -> Maybe String
          -> TRAITS i
          -> PROPERTY i
mkProperty impl t d ts = (buildProperty impl) t d ts

mkSolution : SifBuilder i
          -> String
          -> Maybe String
          -> PROPERTIES i
          -> SOLUTION i
mkSolution impl s d ps = (buildSolution impl) s d ps

-- ------------------------------------------------------------- [ Pattern API ]

mkPattern : SifBuilder i
         -> String
         -> Maybe String
         -> PROBLEM i
         -> SOLUTION i
         -> PATTERN i
mkPattern impl t d p s = (buildPattern impl) t d p s



-- --------------------------------------------------------------------- [ EOF ]
