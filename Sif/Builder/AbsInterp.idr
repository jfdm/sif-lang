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
  PrivmkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> AbsInterpPriv (interpReq t) tyREQ

  PrivmkProb : (title : String)
              -> (desc  : Maybe String)
              -> DList (InterpRes TyREQ) (\x => AbsInterpPriv x TyREQ) xs
              -> AbsInterpPriv (interpProb title xs) TyPROBLEM

  PrivmkTLink : (cval : CValue)
               -> (req : AbsInterpPriv r TyREQ)
               -> (desc : Maybe String)
               -> AbsInterpPriv (interpAffect cval r) TyAFFECTS

  PrivmkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> DList (InterpRes TyAFFECTS) (\x => AbsInterpPriv x TyAFFECTS) rs
               -> AbsInterpPriv (interpTrait title sval rs ty) TyTRAIT

  PrivmkProp : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes TyTRAIT) (\x => AbsInterpPriv x TyTRAIT) ts
              -> AbsInterpPriv (interpProp title ts) TyPROPERTY

  PrivmkSolt : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes TyPROPERTY) (\x => AbsInterpPriv x TyPROPERTY) ps
              -> AbsInterpPriv (interpSolt title ps) TySOLUTION

  PrivmkPatt : (title : String)
              -> (desc : Maybe String)
              -> AbsInterpPriv p TyPROBLEM
              -> AbsInterpPriv s TySOLUTION
              -> AbsInterpPriv (interpPatt p s) TyPATTERN

-- ----------------------------------------------------- [ Instances and Stuff ]


getModel : {x : InterpRes TyPATTERN} -> AbsInterpPriv x TyPATTERN -> GModel
getModel {x} _ = extract x
  where
    extract : InterpRes TyPATTERN -> GModel
    extract (IPatt m) = m
    extract _         = emptyModel

-- ------------------------------------------------------ [ Builder Definition ]

covering
evalPatternAbs : {i : InterpRes TyPATTERN}
              -> AbsInterpPriv i TyPATTERN
              -> Sif.EvalResult
evalPatternAbs p =
    case evalModel (getModel p) Nothing of
      BadModel  => Bad
      Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) (getGNodes gs)


-- Technically, cheating by not propagating the domain across the
-- inner inner type but it is okay IMHO.

data AbsInterpRep : SifTy -> SifDomain -> Type where
  MkWrapper : {i : InterpRes ty} -> AbsInterpPriv i ty -> AbsInterpRep ty d


-- --------------------------------------------------------------- [ Interface ]

getPrivTitle : AbsInterpPriv i ty -> {auto prf : HasMData ty} -> String
getPrivTitle (PrivmkReq _ t _)       = t
getPrivTitle (PrivmkProb t _ _)      = t
getPrivTitle (PrivmkTrait _ t _ _ _) = t
getPrivTitle (PrivmkProp t _ _)      = t
getPrivTitle (PrivmkSolt t _ _)      = t
getPrivTitle (PrivmkPatt t _ _ _)    = t
getPrivTitle _ = "Error"

getPrivDesc : AbsInterpPriv i ty -> Maybe String
getPrivDesc (PrivmkReq _ _ d)       = d
getPrivDesc (PrivmkProb _ d rs)     = d
getPrivDesc (PrivmkTLink _ _ d)     = d
getPrivDesc (PrivmkTrait _ _ d _ _) = d
getPrivDesc (PrivmkProp _ d _)      = d
getPrivDesc (PrivmkSolt _ d _)      = d
getPrivDesc (PrivmkPatt _ d _ _)    = d
getPrivDesc _ = Nothing

getPrivRTy : {i : InterpRes TyREQ}
          -> AbsInterpPriv i TyREQ
          -> RTy
getPrivRTy (PrivmkReq ty _ _) = ty

getPrivTTy : {i : InterpRes TyTRAIT}
          -> AbsInterpPriv i TyTRAIT
          -> TTy
getPrivTTy (PrivmkTrait ty _ _ _ _) = ty

getPrivSValue : {i : InterpRes TyTRAIT}
              -> AbsInterpPriv i TyTRAIT
              -> SValue
getPrivSValue (PrivmkTrait _ _ _ sval as) = sval

getPrivCValue : {i : InterpRes TyAFFECTS}
             -> AbsInterpPriv i TyAFFECTS
             -> CValue
getPrivCValue (PrivmkTLink cval _ _) = cval

getPrivProblem : {i : InterpRes TyPATTERN}
              -> AbsInterpPriv i TyPATTERN
              -> (x : InterpRes TyPROBLEM ** AbsInterpPriv x TyPROBLEM)
getPrivProblem (PrivmkPatt _ _ p _) = (_ ** p)

getPrivSolution : {i : InterpRes TyPATTERN}
               -> AbsInterpPriv i TyPATTERN
               -> (s : InterpRes TySOLUTION ** AbsInterpPriv s TySOLUTION)
getPrivSolution (PrivmkPatt _ _ _ s) = (_ ** s)

getPrivReqs : {i : InterpRes TyPROBLEM}
            -> AbsInterpPriv i TyPROBLEM
            -> (xs ** DList (InterpRes TyREQ) (\x => AbsInterpPriv x TyREQ) xs)
getPrivReqs (PrivmkProb _ _ rs) = (_ ** rs)

getPrivProperties : {i : InterpRes TySOLUTION}
                  -> AbsInterpPriv i TySOLUTION
                  -> (xs ** DList (InterpRes TyPROPERTY) (\x => AbsInterpPriv x TyPROPERTY) xs)
getPrivProperties (PrivmkSolt _ _ ps) = (_ ** ps)

getPrivTraits : {i : InterpRes TyPROPERTY}
             -> AbsInterpPriv i TyPROPERTY
             -> (xs ** DList (InterpRes TyTRAIT) (\x => AbsInterpPriv x TyTRAIT) xs)
getPrivTraits (PrivmkProp _ _ ts) = (_ ** ts)

getPrivAffects : {i : InterpRes TyTRAIT}
               -> AbsInterpPriv i TyTRAIT
               -> (xs ** DList (InterpRes TyAFFECTS) (\x => AbsInterpPriv x TyAFFECTS) xs)
getPrivAffects (PrivmkTrait _ _ _ _ as) = (_ ** as)

getPrivReq : {i : InterpRes TyAFFECTS}
          -> AbsInterpPriv i TyAFFECTS
          -> (s : InterpRes TyREQ ** AbsInterpPriv s TyREQ)
getPrivReq (PrivmkTLink _ r _) = (_ ** r)

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
buildReqAbs _ ty s d = MkExpr $ MkWrapper $ PrivmkReq ty s d

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
    MkExpr $ MkWrapper $ PrivmkProb t d (Sigma.getProof $ convS rs)

covering
buildAffectAbs : (d : SifDomain)
              -> CValue
              -> REQUIREMENT AbsInterpRep d
              -> Maybe String
              -> AFFECT AbsInterpRep d
buildAffectAbs _ c r d =
    MkExpr $ MkWrapper $ PrivmkTLink c
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
    MkExpr $ MkWrapper $ PrivmkTrait ty t d s (Sigma.getProof $ convS rs)

covering
buildPropertyAbs : (d : SifDomain)
                -> String
                -> Maybe String
                -> TRAITS AbsInterpRep d
                -> PROPERTY AbsInterpRep d
buildPropertyAbs _ t d ts =
    MkExpr $ MkWrapper $ PrivmkProp t d (Sigma.getProof $ convS ts)

covering
buildSolutionAbs : (d : SifDomain)
                -> String
                -> Maybe String
                -> PROPERTIES AbsInterpRep d
                -> SOLUTION AbsInterpRep d
buildSolutionAbs _ s d ps =
    MkExpr $ MkWrapper $ PrivmkSolt s d
           (Sigma.getProof $ convS ps)

covering
buildPatternAbs : (d : SifDomain)
               -> String
               -> Maybe String
               -> PROBLEM AbsInterpRep  d
               -> SOLUTION AbsInterpRep d
               -> PATTERN AbsInterpRep  d
buildPatternAbs _ t d (MkExpr (MkWrapper p)) (MkExpr (MkWrapper s)) =
    MkExpr $ MkWrapper $ PrivmkPatt t d p s

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
