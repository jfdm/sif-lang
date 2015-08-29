module Sif.Pattern

import Data.GraphViz.SimpleDot

import public GRL.Lang.GLang
import Edda
import XML.DOM

import Sif.Types

%default partial
%access public

-- --------------------------------------------------- [ Public Data Structure ]

covering
convTy : SifOutFormat -> Type
convTy ORG     = String
convTy XML     = Document DOCUMENT
convTy DOT     = SimpleDot GRAPH
convTy GRL     = GModel
convTy EDDA    = Edda PRIME MODEL
convTy COMPACT = String
convTy IDRIS   = String

namespace Sif
  data EvalResult : Type where
    Good : List (String, Maybe SValue) -> EvalResult
    Bad  : EvalResult

||| The Representation API
class SifRepAPI (impl : SifTy -> Type) where
    getTitle    : impl ty -> Maybe String
    evalPattern : impl tyPATTERN -> Sif.EvalResult
    toString    : impl ty -> String
    convTo      : impl tyPATTERN -> (ty : SifOutFormat) -> Maybe $ convTy ty

data SifExpr : (impl : SifTy -> Type) -> SifTy -> Type where
  MkExpr : SifRepAPI impl => impl ty -> SifExpr impl ty

instance SifRepAPI (\ty => SifExpr l ty) where
  getTitle (MkExpr x) = getTitle x
  evalPattern (MkExpr x) = evalPattern x
  toString (MkExpr x)    = toString x
  convTo (MkExpr x) fmt  = convTo x fmt

instance Show (SifExpr l ty) where
  show x = toString x

||| o'rrible code
covering
showConvPattern : SifExpr impl tyPATTERN -> SifOutFormat -> Maybe String
showConvPattern p GRL =
  case (the (Maybe (convTy GRL)) (convTo p GRL)) of
    Nothing => Nothing
    Just r  => Just (show r)
showConvPattern p DOT =
  case (the (Maybe (convTy DOT)) (convTo p DOT)) of
    Nothing => Nothing
    Just r  => Just (show r)
showConvPattern p XML =
  case (the (Maybe (convTy XML)) (convTo p XML)) of
    Nothing => Nothing
    Just r  => Just (show @{xml} r)
showConvPattern p ORG =
  case (the (Maybe (convTy ORG)) (convTo p ORG)) of
    Nothing => Nothing
    Just r  => Just (show r)
showConvPattern p COMPACT =
  case (the (Maybe (convTy COMPACT)) (convTo p COMPACT)) of
    Nothing => Nothing
    Just r  => Just (show r)
showConvPattern p IDRIS =
  case (the (Maybe (convTy IDRIS)) (convTo p IDRIS)) of
    Nothing => Nothing
    Just r  => Just (show r)
showConvPattern p EDDA =
  case (the (Maybe (convTy EDDA)) (convTo p EDDA)) of
    Nothing => Nothing
    Just r  => Nothing
showConvPattern _ _ = Nothing

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

-- ------------------------------------------------------ [ Factory Definition ]

||| Factories for building concrete representations
record SifBuilder (impl : SifTy -> Type) where
  constructor MkSifBuilder
  buildReq     : RTy -> String -> Maybe String -> REQUIREMENT impl
  buildProblem : String -> Maybe String -> REQUIREMENTS impl -> PROBLEM impl

  buildAffect   : CValue -> REQUIREMENT impl -> Maybe String -> AFFECT impl
  buildTrait    : TTy -> String -> Maybe String -> SValue -> AFFECTS impl -> TRAIT impl
  buildProperty : String -> Maybe String -> TRAITS impl -> PROPERTY impl
  buildSolution : String -> Maybe String -> PROPERTIES impl -> SOLUTION impl

  buildPattern  : String -> Maybe String -> PROBLEM impl -> SOLUTION impl -> PATTERN impl

record SifBackend where
    constructor MkBackend
    name    : String
    builder : (SifBuilder impl)

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
