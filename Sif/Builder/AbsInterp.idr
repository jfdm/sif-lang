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


data AbsInterpRep : SifTy -> Type where
  MkWrapper : {i : InterpRes ty} -> AbsInterpPriv i ty -> AbsInterpRep ty


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
getPrivDesc (priv__mkTLink _ _ d)    = d
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
buildReqAbs : RTy -> String -> Maybe String -> REQUIREMENT AbsInterpRep
buildReqAbs ty s d = MkExpr $ MkWrapper $ priv__mkReq ty s d

%inline covering
conv : List (SifExpr AbsInterpRep ty)
    -> (xs ** DList (InterpRes ty) (\x => AbsInterpPriv x ty) xs)
conv xs = fromLDP $ map (\(MkExpr (MkWrapper x)) => (_ ** x)) xs

%inline covering
convR : SifExpr AbsInterpRep tyREQ -> (r : InterpRes tyREQ ** AbsInterpPriv r tyREQ)
convR (MkExpr (MkWrapper res)) = (_ ** res)


covering
buildProblemAbs : String -> Maybe String -> REQUIREMENTS AbsInterpRep -> PROBLEM AbsInterpRep
buildProblemAbs t d rs =
    MkExpr $ MkWrapper $ priv__mkProb t d (Sigma.getProof $ conv rs)

covering
buildAffectAbs : CValue -> REQUIREMENT AbsInterpRep -> Maybe String -> AFFECT AbsInterpRep
buildAffectAbs c r d = MkExpr $ MkWrapper $ priv__mkTLink c (Sigma.getProof $ convR r) d

covering
buildTraitAbs : TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS AbsInterpRep
             -> TRAIT AbsInterpRep
buildTraitAbs ty t d s rs =
    MkExpr $ MkWrapper $ priv__mkTrait ty t d s (Sigma.getProof $ conv rs)

covering
buildPropertyAbs : String -> Maybe String -> TRAITS AbsInterpRep -> PROPERTY AbsInterpRep
buildPropertyAbs t d ts =
    MkExpr $ MkWrapper $ priv__mkProp t d (Sigma.getProof $ conv ts)

covering
buildSolutionAbs : String -> Maybe String -> PROPERTIES AbsInterpRep -> SOLUTION AbsInterpRep
buildSolutionAbs s d ps =
    MkExpr $ MkWrapper $ priv__mkSolt s d (Sigma.getProof $ conv ps)

covering
buildPatternAbs  : String -> Maybe String -> PROBLEM AbsInterpRep -> SOLUTION AbsInterpRep -> PATTERN AbsInterpRep
buildPatternAbs t d (MkExpr (MkWrapper p)) (MkExpr (MkWrapper s)) = MkExpr $ MkWrapper $ priv__mkPatt t d p s

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
