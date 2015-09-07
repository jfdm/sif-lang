-- ----------------------------------------------------------- [ AbsInterp.idr ]
-- Module    : AbsInterp.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.AbsInterp

import Effects
import Effect.State

import Data.AVL.Dict

import GRL.Lang.GLang
import GRL.Eval

import Sif.Types
import Sif.Pattern
import Sif.Builder.Utils

-- -------------------------------------------------------------- [ Directives ]

-- %access private
%default total

-- ----------------------------------------- [ Private Internal Data Structure ]

data AbsInterpPriv : InterpRes ty -> SifTy -> Type where
  priv__mkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> AbsInterpPriv (interpReq t) tyREQ

  priv__mkProb : (title : String)
              -> (desc  : Maybe String)
              -> DList (InterpRes tyREQ) (\x => AbsInterpPriv x tyREQ) xs
              -> AbsInterpPriv (interpProb title xs) tyPROBLEM

  priv__mkTLink : (cval : CValue)
               -> (req : AbsInterpPriv r tyREQ)
               -> (desc : Maybe String)
               -> AbsInterpPriv (interpAffect cval r) tyAFFECTS

  priv__mkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> DList (InterpRes tyAFFECTS) (\x => AbsInterpPriv x tyAFFECTS) rs
               -> AbsInterpPriv (interpTrait title sval rs ty) tyTRAIT

  priv__mkProp : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyTRAIT) (\x => AbsInterpPriv x tyTRAIT) ts
              -> AbsInterpPriv (interpProp title ts) tyPROPERTY

  priv__mkSolt : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyPROPERTY) (\x => AbsInterpPriv x tyPROPERTY) ps
              -> AbsInterpPriv (interpSolt title ps) tySOLUTION

  priv__mkPatt : (title : String)
              -> (desc : Maybe String)
              -> AbsInterpPriv p tyPROBLEM
              -> AbsInterpPriv s tySOLUTION
              -> AbsInterpPriv (interpPatt p s) tyPATTERN

-- ----------------------------------------------------- [ Instances and Stuff ]


getModel : {x : InterpRes tyPATTERN} -> AbsInterpPriv x tyPATTERN -> GModel
getModel {x} _ = extract x
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m
    extract _         = emptyModel

-- ------------------------------------------------------ [ Builder Definition ]

covering
evalPatternAbs : {i : InterpRes tyPATTERN}
              -> AbsInterpPriv i tyPATTERN
              -> Sif.EvalResult
evalPatternAbs p =
    case evalModel (getModel p) Nothing of
      BadModel  => Bad
      Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) gs


-- Technically, cheating by not propagating the domain across the
-- inner inner type but it is okay IMHO.

data AbsInterpRep : SifTy -> SifDomain -> Type where
  MkWrapper : {i : InterpRes ty} -> AbsInterpPriv i ty -> AbsInterpRep ty d


-- --------------------------------------------------------------- [ Interface ]

getPrivTitle : AbsInterpPriv i ty -> {auto prf : HasMData ty} -> String
getPrivTitle (priv__mkReq _ t _)       = t
getPrivTitle (priv__mkProb t _ _)      = t
getPrivTitle (priv__mkTrait _ t _ _ _) = t
getPrivTitle (priv__mkProp t _ _)      = t
getPrivTitle (priv__mkSolt t _ _)      = t
getPrivTitle (priv__mkPatt t _ _ _)    = t
getPrivTitle _ = "Error"

getPrivDesc : AbsInterpPriv i ty -> Maybe String
getPrivDesc (priv__mkReq _ _ d)       = d
getPrivDesc (priv__mkProb _ d rs)     = d
getPrivDesc (priv__mkTLink _ _ d)     = d
getPrivDesc (priv__mkTrait _ _ d _ _) = d
getPrivDesc (priv__mkProp _ d _)      = d
getPrivDesc (priv__mkSolt _ d _)      = d
getPrivDesc (priv__mkPatt _ d _ _)    = d
getPrivDesc _ = Nothing


getPrivRTy : AbsInterpPriv i tyREQ -> RTy
getPrivRTy (priv__mkReq ty _ _) = ty

getPrivTTy    : AbsInterpPriv i tyTRAIT -> TTy
getPrivTTy (priv__mkTrait ty _ _ _ _) = ty

getPrivSValue : AbsInterpPriv i tyTRAIT -> SValue
getPrivSValue (priv__mkTrait _ _ _ sval as) = sval

getPrivCValue : AbsInterpPriv i tyAFFECTS -> CValue
getPrivCValue (priv__mkTLink cval _ _) = cval

getPrivProblem : AbsInterpPriv i tyPATTERN
    -> (x : InterpRes tyPROBLEM ** AbsInterpPriv x tyPROBLEM)
getPrivProblem (priv__mkPatt _ _ p _) = (_ ** p)

getPrivSolution   : AbsInterpPriv i tyPATTERN
    -> (s : InterpRes tySOLUTION ** AbsInterpPriv s tySOLUTION)
getPrivSolution (priv__mkPatt _ _ _ s) = (_ ** s)

getPrivReqs : AbsInterpPriv i tyPROBLEM
  -> (xs ** DList (InterpRes tyREQ) (\x => AbsInterpPriv x tyREQ) xs)
getPrivReqs (priv__mkProb _ _ rs) = (_ ** rs)

getPrivProperties : AbsInterpPriv i tySOLUTION
  -> (xs ** DList (InterpRes tyPROPERTY) (\x => AbsInterpPriv x tyPROPERTY) xs)
getPrivProperties (priv__mkSolt _ _ ps) = (_ ** ps)

getPrivTraits : AbsInterpPriv i tyPROPERTY
  -> (xs ** DList (InterpRes tyTRAIT) (\x => AbsInterpPriv x tyTRAIT) xs)
getPrivTraits (priv__mkProp _ _ ts) = (_ ** ts)

getPrivAffects : AbsInterpPriv i tyTRAIT
  -> (xs ** DList (InterpRes tyAFFECTS) (\x => AbsInterpPriv x tyAFFECTS) xs)
getPrivAffects (priv__mkTrait _ _ _ _ as) = (_ ** as)

getPrivReq : AbsInterpPriv i tyAFFECTS -> (s : InterpRes tyREQ ** AbsInterpPriv s tyREQ)
getPrivReq (priv__mkTLink _ r _) = (_ ** r)

instance SifRepAPI AbsInterpRep where
  getTitle  (MkWrapper i) = getPrivTitle i
  getDesc   (MkWrapper i) = getPrivDesc  i
  getTTy    (MkWrapper x) = getPrivTTy x
  getRTy    (MkWrapper x) = getPrivRTy x
  getSValue (MkWrapper x) = getPrivSValue x
  getCValue (MkWrapper x) = getPrivCValue x

  getProblem    (MkWrapper x) = MkWrapper $ Sigma.getProof $ getPrivProblem x
  getSolution   (MkWrapper x) = MkWrapper $ Sigma.getProof $ getPrivSolution x

  getReqs       (MkWrapper x) = map (\(prf ** i) => MkWrapper i) $ toLDP $  (Sigma.getProof (getPrivReqs x))
  getProperties (MkWrapper x) = map (\(prf ** i) => MkWrapper i) $ toLDP $  (Sigma.getProof (getPrivProperties x))
  getTraits     (MkWrapper x) = map (\(prf ** i) => MkWrapper i) $ toLDP $  (Sigma.getProof (getPrivTraits x))
  getAffects    (MkWrapper x) = map (\(prf ** i) => MkWrapper i) $ toLDP $ (Sigma.getProof (getPrivAffects x))
  getReq        (MkWrapper x) = MkWrapper $ Sigma.getProof $ getPrivReq x

  evalPattern (MkWrapper p)    = evalPatternAbs p
  fetchMetaModel (MkWrapper p) = m
      where
        g : GModel
        g = getModel p

        m : MetaModel
        m = MkModel g

-- ------------------------------------------------------------- [ The Builder ]

covering
buildReqAbs : (d : SifDomain)
           -> RTy
           -> String
           -> Maybe String
           -> REQUIREMENT AbsInterpRep d
buildReqAbs _ ty s d = MkExpr $ MkWrapper $ priv__mkReq ty s d

%inline covering
convS : List (SifExpr ty d AbsInterpRep)
    -> (xs : List (InterpRes ty) ** DList (InterpRes ty) (\x => AbsInterpPriv x ty) xs)
convS xs = fromLDP $ map (\(MkExpr (MkWrapper x)) => (_ ** x)) xs

%inline covering
conv : {ty : SifTy}
    -> SifExpr ty d AbsInterpRep
    -> (r : InterpRes ty ** AbsInterpPriv r ty)
conv (MkExpr (MkWrapper res)) = (_ ** res)


covering
buildProblemAbs : (d : SifDomain)
               -> String
               -> Maybe String
               -> REQUIREMENTS AbsInterpRep d
               -> PROBLEM AbsInterpRep d
buildProblemAbs c t d rs =
    MkExpr $ MkWrapper $ priv__mkProb t d (Sigma.getProof $ convS rs)

covering
buildAffectAbs : (d : SifDomain)
              -> CValue
              -> REQUIREMENT AbsInterpRep d
              -> Maybe String
              -> AFFECT AbsInterpRep d
buildAffectAbs _ c r d =
    MkExpr $ MkWrapper $ priv__mkTLink c
           (Sigma.getProof $ conv r) d

covering
buildTraitAbs : (d : SifDomain)
             -> TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS AbsInterpRep d
             -> TRAIT AbsInterpRep d
buildTraitAbs _ ty t d s rs =
    MkExpr $ MkWrapper $ priv__mkTrait ty t d s (Sigma.getProof $ convS rs)

covering
buildPropertyAbs : (d : SifDomain)
                -> String
                -> Maybe String
                -> TRAITS AbsInterpRep d
                -> PROPERTY AbsInterpRep d
buildPropertyAbs _ t d ts =
    MkExpr $ MkWrapper $ priv__mkProp t d (Sigma.getProof $ convS ts)

covering
buildSolutionAbs : (d : SifDomain)
                -> String
                -> Maybe String
                -> PROPERTIES AbsInterpRep d
                -> SOLUTION AbsInterpRep d
buildSolutionAbs _ s d ps =
    MkExpr $ MkWrapper $ priv__mkSolt s d
           (Sigma.getProof $ convS ps)

covering
buildPatternAbs : (d : SifDomain)
               -> String
               -> Maybe String
               -> PROBLEM AbsInterpRep  d
               -> SOLUTION AbsInterpRep d
               -> PATTERN AbsInterpRep  d
buildPatternAbs _ t d (MkExpr (MkWrapper p)) (MkExpr (MkWrapper s)) =
    MkExpr $ MkWrapper $ priv__mkPatt t d p s

covering
absInterpBuilder : SifBuilder AbsInterpRep
absInterpBuilder = MkSifBuilder
    buildReqAbs
    buildProblemAbs
    buildAffectAbs
    buildTraitAbs
    buildPropertyAbs
    buildSolutionAbs
    buildPatternAbs

covering
backendAbsInterp : SifBackend
backendAbsInterp = MkBackend "interp" absInterpBuilder

-- --------------------------------------------------------------------- [ EOF ]
