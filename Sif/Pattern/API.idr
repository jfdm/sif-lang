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

FUNCTIONAL : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
FUNCTIONAL impl d = SifExpr impl d tyREQ

USABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
USABILITY impl d = SifExpr impl d tyREQ

RELIABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
RELIABILITY impl d = SifExpr impl d tyREQ

PERFORMANCE : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PERFORMANCE impl d = SifExpr impl d tyREQ

SUPPORTABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
SUPPORTABILITY impl d = SifExpr impl d tyREQ

REQUIREMENT : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
REQUIREMENT impl d = SifExpr impl d tyREQ

REQUIREMENTS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
REQUIREMENTS impl d = List (REQUIREMENT impl d)

PROBLEM : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROBLEM impl d = SifExpr impl d tyPROBLEM

ADVANTAGE : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
ADVANTAGE impl d = SifExpr impl d tyTRAIT

DISADVANTAGE : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
DISADVANTAGE impl d = SifExpr impl d tyTRAIT

TRAIT : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
TRAIT impl d = SifExpr impl d tyTRAIT

TRAITS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
TRAITS impl d = List (TRAIT impl d)

AFFECT : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
AFFECT impl d = SifExpr impl d tyAFFECTS

AFFECTS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
AFFECTS impl d = List (AFFECT impl d)

PROPERTY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROPERTY impl d = SifExpr impl d tyPROPERTY

PROPERTIES : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROPERTIES impl d = List (PROPERTY impl d)

SOLUTION : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
SOLUTION impl d = SifExpr impl d tySOLUTION

PATTERN : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PATTERN impl d = SifExpr impl d tyPATTERN

-- ------------------------------------------------------------- [ Problem API ]

mkRequirement : SifBuilder i d
             -> RTy
             -> String
             -> Maybe String
             -> REQUIREMENT i d
mkRequirement impl ty s desc = (buildReq impl) ty s desc

mkFunctional : SifBuilder i d
            -> String
            -> Maybe String
            -> FUNCTIONAL i d
mkFunctional impl s desc = (buildReq impl) FUNC s desc

mkUsability : SifBuilder i d
           -> String
           -> Maybe String
           -> USABILITY i d
mkUsability impl s desc = (buildReq impl) USAB s desc

mkReliability : SifBuilder i d
             -> String
             -> Maybe String
             -> RELIABILITY i d
mkReliability impl s desc = (buildReq impl) RELI s desc

mkPerformance : SifBuilder i d
             -> String
             -> Maybe String
             -> PERFORMANCE i d
mkPerformance impl s desc = (buildReq impl) PERF s desc

mkSupportability : SifBuilder i d
                -> String
                -> Maybe String
                -> SUPPORTABILITY i d
mkSupportability impl s desc = (buildReq impl) SUPP s desc

mkProblem : SifBuilder i d
         -> String
         -> Maybe String
         -> REQUIREMENTS i d
         -> PROBLEM i d
mkProblem impl s d rs = (buildProblem impl) s d rs

-- ------------------------------------------------------------ [ Solution API ]

mkAffect : SifBuilder i d
        -> CValue
        -> REQUIREMENT i d
        -> Maybe String
        -> AFFECT i d
mkAffect impl c r d = (buildAffect impl) c r d

mkTrait : SifBuilder i d
       -> TTy
       -> String
       -> Maybe String
       -> SValue
       -> AFFECTS i d
       -> TRAIT i d
mkTrait impl ty t d s rs = (buildTrait impl) ty t d s' rs
  where
    s' : SValue
    s' = case ty of {GEN => s; ADV => s; DIS => invertEval s}

mkAspect : SifBuilder i d
        -> String
        -> Maybe String
        -> SValue
        -> AFFECTS i d
        -> TRAIT i d
mkAspect impl t d s rs = (buildTrait impl) GEN t d s rs

mkAdvantage : SifBuilder i d
           -> String
           -> Maybe String
           -> SValue
           -> AFFECTS i d
           -> ADVANTAGE i d
mkAdvantage impl t d s rs = (buildTrait impl) ADV t d s rs

mkDisadvantage : SifBuilder i d
              -> String
              -> Maybe String
              -> SValue
              -> AFFECTS i d
              -> DISADVANTAGE i d
mkDisadvantage impl t d s rs = (buildTrait impl) DIS t d (invertEval s) rs

mkProperty : SifBuilder i d
          -> String
          -> Maybe String
          -> TRAITS i d
          -> PROPERTY i d
mkProperty impl t d ts = (buildProperty impl) t d ts

mkSolution : SifBuilder i d
          -> String
          -> Maybe String
          -> PROPERTIES i d
          -> SOLUTION i d
mkSolution impl s d ps = (buildSolution impl) s d ps

-- ------------------------------------------------------------- [ Pattern API ]

mkPattern : SifBuilder i d
         -> String
         -> Maybe String
         -> PROBLEM i d
         -> SOLUTION i d
         -> PATTERN i d
mkPattern impl t d p s = (buildPattern impl) t d p s



-- --------------------------------------------------------------------- [ EOF ]
