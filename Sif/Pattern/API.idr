-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.API

import GRL.Lang.GLang

import Sif.Types
import Sif.Pattern.Model

%access export
-- ----------------------------------------------------------- [ Type Synonyms ]

public export
FUNCTIONAL : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
FUNCTIONAL impl d = SifExpr TyREQ d impl

public export
USABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
USABILITY impl d = SifExpr TyREQ d impl

public export
RELIABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
RELIABILITY impl d = SifExpr TyREQ d impl

public export
PERFORMANCE : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PERFORMANCE impl d = SifExpr TyREQ d impl

public export
SUPPORTABILITY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
SUPPORTABILITY impl d = SifExpr TyREQ d impl

public export
REQUIREMENT : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
REQUIREMENT impl d = SifExpr TyREQ d impl

public export
REQUIREMENTS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
REQUIREMENTS impl d = List (REQUIREMENT impl d)

public export
PROBLEM : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROBLEM impl d = SifExpr TyPROBLEM d impl

public export
ADVANTAGE : (impl : SifTy -> SifDomain -> Type) -> SifDomain -> Type
ADVANTAGE impl d = SifExpr TyTRAIT d impl

public export
DISADVANTAGE : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
DISADVANTAGE impl d = SifExpr TyTRAIT d impl

public export
TRAIT : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
TRAIT impl d = SifExpr TyTRAIT d impl

public export
TRAITS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
TRAITS impl d = List (TRAIT impl d)

public export
AFFECT : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
AFFECT impl d = SifExpr TyAFFECTS d impl

public export
AFFECTS : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
AFFECTS impl d = List (AFFECT impl d)

public export
PROPERTY : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROPERTY impl d = SifExpr TyPROPERTY d impl

public export
PROPERTIES : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PROPERTIES impl d = List (PROPERTY impl d)

public export
SOLUTION : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
SOLUTION impl d = SifExpr TySOLUTION d impl

public export
PATTERN : (SifTy -> SifDomain -> Type) -> SifDomain -> Type
PATTERN impl d = SifExpr TyPATTERN d impl

-- ------------------------------------------------------------- [ Problem API ]

mkRequirement : SifBuilder i
             -> (d : SifDomain)
             -> RTy
             -> String
             -> Maybe String
             -> REQUIREMENT i d
mkRequirement impl d ty s desc = (buildReq impl) d ty s desc

mkFunctional : SifBuilder i
            -> (d : SifDomain)
            -> String
            -> Maybe String
            -> FUNCTIONAL i d
mkFunctional impl d s desc = (buildReq impl) d FUNC s desc

mkUsability : SifBuilder i
           -> (d : SifDomain)
           -> String
           -> Maybe String
           -> USABILITY i d
mkUsability impl d s desc = (buildReq impl) d USAB s desc

mkReliability : SifBuilder i
             -> (d : SifDomain)
             -> String
             -> Maybe String
             -> RELIABILITY i d
mkReliability impl d s desc = (buildReq impl) d RELI s desc

mkPerformance : SifBuilder i
             -> (d : SifDomain)
             -> String
             -> Maybe String
             -> PERFORMANCE i d
mkPerformance impl d s desc = (buildReq impl) d PERF s desc

mkSupportability : SifBuilder i
                -> (d : SifDomain)
                -> String
                -> Maybe String
                -> SUPPORTABILITY i d
mkSupportability impl d s desc = (buildReq impl) d SUPP s desc

mkProblem : SifBuilder i
         -> (d : SifDomain)
         -> String
         -> Maybe String
         -> REQUIREMENTS i d
         -> PROBLEM i d
mkProblem impl d t desc rs = (buildProblem impl) d t desc rs

-- ------------------------------------------------------------ [ Solution API ]

mkAffect : SifBuilder i
        -> (d : SifDomain)
        -> CValue
        -> REQUIREMENT i d
        -> Maybe String
        -> AFFECT i d
mkAffect impl d c r desc = (buildAffect impl) d c r desc

mkTrait : SifBuilder i
       -> (d : SifDomain)
       -> TTy
       -> String
       -> Maybe String
       -> SValue
       -> AFFECTS i d
       -> TRAIT i d
mkTrait impl d ty t desc s rs = (buildTrait impl) d ty t desc s' rs
  where
    s' : SValue
    s' = case ty of {GEN => s; ADV => s; DIS => invertEval s}

mkAspect : SifBuilder i
        -> (d : SifDomain)
        -> String
        -> Maybe String
        -> SValue
        -> AFFECTS i d
        -> TRAIT i d
mkAspect impl d t desc s rs = (buildTrait impl) d GEN t desc s rs

mkAdvantage : SifBuilder i
           -> (d : SifDomain)
           -> String
           -> Maybe String
           -> SValue
           -> AFFECTS i d
           -> ADVANTAGE i d
mkAdvantage impl d t desc s rs = (buildTrait impl) d ADV t desc s rs

mkDisadvantage : SifBuilder i
              -> (d : SifDomain)
              -> String
              -> Maybe String
              -> SValue
              -> AFFECTS i d
              -> DISADVANTAGE i d
mkDisadvantage impl d t desc s rs = (buildTrait impl) d DIS t desc (invertEval s) rs

mkProperty : SifBuilder i
          -> (d : SifDomain)
          -> String
          -> Maybe String
          -> TRAITS i d
          -> PROPERTY i d
mkProperty impl d t desc ts = (buildProperty impl) d t desc ts

mkSolution : SifBuilder i
          -> (d : SifDomain)
          -> String
          -> Maybe String
          -> PROPERTIES i d
          -> SOLUTION i d
mkSolution impl d t desc ps = (buildSolution impl) d t desc ps

-- ------------------------------------------------------------- [ Pattern API ]

mkPattern : SifBuilder i
         -> (d : SifDomain)
         -> String
         -> Maybe String
         -> PROBLEM i d
         -> SOLUTION i d
         -> PATTERN i d
mkPattern impl d t desc p s = (buildPattern impl) d t desc p s



-- --------------------------------------------------------------------- [ EOF ]
