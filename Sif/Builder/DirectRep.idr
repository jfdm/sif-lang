-- -------------------------------------------------------- [ DirectInterp.idr ]
-- Module    : DirectInterp.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.DirectRep

import Data.AVL.Dict
import Data.GraphViz.SimpleDot

import GRL.Lang.GLang
import GRL.Eval

import Edda
import Edda.Reader.Org
import XML.DOM

import Sif.Types
import Sif.Pattern
import Sif.Builder.Utils

-- -------------------------------------------------------------- [ Directives ]

%access public
%default partial

-- ----------------------------------------- [ Private Internal Data Structure ]

data DirectRep : SifTy -> SifDomain -> Type where
  DirectMkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> DirectRep TyREQ d

  DirectMkProb : (title : String)
              -> (desc  : Maybe String)
              -> List (DirectRep TyREQ d)
              -> DirectRep TyPROBLEM d

  DirectMkAffect : (cval : CValue)
               -> (req : DirectRep TyREQ d)
               -> (desc : Maybe String)
               -> DirectRep TyAFFECTS d

  DirectMkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> List (DirectRep TyAFFECTS d)
               -> DirectRep TyTRAIT d

  DirectMkProp : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep TyTRAIT d)
              -> DirectRep TyPROPERTY d

  DirectMkSolt : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep TyPROPERTY d)
              -> DirectRep TySOLUTION d

  DirectMkPatt : (title : String)
              -> (desc : Maybe String)
              -> DirectRep TyPROBLEM  d
              -> DirectRep TySOLUTION d
              -> DirectRep TyPATTERN  d

getDirectTitle : DirectRep ty d -> String
getDirectTitle (DirectMkReq _ t _)       = t
getDirectTitle (DirectMkProb t _ _)      = t
getDirectTitle (DirectMkTrait _ t _ _ _) = t
getDirectTitle (DirectMkProp t _ _)      = t
getDirectTitle (DirectMkSolt t _ _)      = t
getDirectTitle (DirectMkPatt t _ _ _)    = t


getDirectDesc : DirectRep ty d -> Maybe String
getDirectDesc (DirectMkReq _ _ d)       = d
getDirectDesc (DirectMkProb _ d rs)     = d
getDirectDesc (DirectMkAffect _ _ d)    = d
getDirectDesc (DirectMkTrait _ _ d _ _) = d
getDirectDesc (DirectMkProp _ d _)      = d
getDirectDesc (DirectMkSolt _ d _)      = d
getDirectDesc (DirectMkPatt _ d _ _)    = d

getDirectRTy : DirectRep TyREQ d -> RTy
getDirectRTy (DirectMkReq ty _ _) = ty

getDirectTTy : DirectRep TyTRAIT d -> TTy
getDirectTTy (DirectMkTrait ty _ _ _ _) = ty

getDirectSValue : DirectRep TyTRAIT d -> SValue
getDirectSValue (DirectMkTrait _ _ _ sval _) = sval

getDirectCValue : DirectRep TyAFFECTS d -> CValue
getDirectCValue (DirectMkAffect cval _ _) = cval

getDirectProblem : DirectRep TyPATTERN d -> (DirectRep TyPROBLEM d)
getDirectProblem (DirectMkPatt _ _ p _) = p

getDirectSolution : DirectRep TyPATTERN d -> DirectRep TySOLUTION d
getDirectSolution (DirectMkPatt _ _ _ s) = s

getDirectReqs : DirectRep TyPROBLEM d -> List $ DirectRep TyREQ d
getDirectReqs (DirectMkProb _ _ rs) = rs

getDirectProperties : DirectRep TySOLUTION d -> List $ DirectRep TyPROPERTY d
getDirectProperties (DirectMkSolt _ _ ps) = ps

getDirectTraits : DirectRep TyPROPERTY d -> List $ DirectRep TyTRAIT d
getDirectTraits (DirectMkProp _ _ ts) = ts

getDirectAffects : DirectRep TyTRAIT d -> List $ DirectRep TyAFFECTS d
getDirectAffects (DirectMkTrait _ _ _ _ as) = as

getDirectReq : DirectRep TyAFFECTS d -> DirectRep TyREQ d
getDirectReq (DirectMkAffect _ r _) = r

-- --------------------------------------------------------------------- [ GRL ]

covering
toGRL : DirectRep ty d -> InterpRes ty
toGRL (DirectMkReq ty t d)        = interpReq t
toGRL (DirectMkProb t d rs)       = interpProb t (map toGRL rs)
toGRL (DirectMkAffect cval r d)   = interpAffect cval (toGRL r)
toGRL (DirectMkTrait ty t d s rs) = interpTrait t s (map toGRL rs) ty
toGRL (DirectMkProp t d ts)       = interpProp t (map toGRL ts)
toGRL (DirectMkSolt t d ps)       = interpSolt t (map toGRL ps)
toGRL (DirectMkPatt t d p s)      = interpPatt (toGRL p) (toGRL s)


-- ----------------------------------------------------- [ Instances and Stuff ]

getGModel : DirectRep TyPATTERN d -> GModel
getGModel p = extract (toGRL p)
  where
    extract : InterpRes TyPATTERN -> GModel
    extract (IPatt m) = m

-- ------------------------------------------------------ [ Builder Definition ]

instance SifRepAPI DirectRep where
  getTitle  x = getDirectTitle x
  getDesc     = getDirectDesc
  getTTy      = getDirectTTy
  getRTy      = getDirectRTy
  getSValue   = getDirectSValue
  getCValue   = getDirectCValue

  getProblem    = getDirectProblem
  getSolution   = getDirectSolution
  getReqs       = getDirectReqs
  getProperties = getDirectProperties
  getTraits     = getDirectTraits
  getAffects    = getDirectAffects
  getReq        = getDirectReq

  evalPattern p =
      case evalModel (getGModel p) Nothing of
        BadModel  => Bad
        Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) gs

  fetchMetaModel p = MkModel $ getGModel p

-- ------------------------------------------------------------- [ The Builder ]

%inline
conv : SifExpr ty d DirectRep -> DirectRep ty d
conv (MkExpr x) = x

buildReqDir : (d : SifDomain)
           -> RTy
           -> String
           -> Maybe String
           -> REQUIREMENT DirectRep d
buildReqDir _ ty s d = MkExpr $ DirectMkReq ty s d

buildProblemDir : (d : SifDomain)
               -> String
               -> Maybe String
               -> REQUIREMENTS DirectRep d
               -> PROBLEM DirectRep d
buildProblemDir _ t d rs =
    MkExpr $ DirectMkProb t d (map conv rs)

buildAffectDir : (d : SifDomain)
              -> CValue
              -> REQUIREMENT DirectRep d
              -> Maybe String
              -> AFFECT DirectRep d
buildAffectDir _ c r d = MkExpr $ DirectMkAffect c (conv r) d

buildTraitDir : (d : SifDomain)
             -> TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS DirectRep d
             -> TRAIT DirectRep d
buildTraitDir _ ty t d s rs =
    MkExpr $ DirectMkTrait ty t d s (map conv rs)


buildPropertyDir : (d : SifDomain)
                -> String
                -> Maybe String
                -> TRAITS DirectRep d
                -> PROPERTY DirectRep d
buildPropertyDir _ t d ts =
    MkExpr $ DirectMkProp t d (map conv ts)

buildSolutionDir : (d : SifDomain)
                -> String
                -> Maybe String
                -> PROPERTIES DirectRep d
                -> SOLUTION DirectRep d
buildSolutionDir _ s d ps =
    MkExpr $ DirectMkSolt s d (map conv ps)

buildPatternDir : (d : SifDomain)
               -> String
               -> Maybe String
               -> PROBLEM DirectRep d
               -> SOLUTION DirectRep d
               -> PATTERN DirectRep d
buildPatternDir _ t d p s = MkExpr $ DirectMkPatt t d (conv p) (conv s)

directBuilder : SifBuilder DirectRep
directBuilder = MkSifBuilder
    buildReqDir
    buildProblemDir
    buildAffectDir
    buildTraitDir
    buildPropertyDir
    buildSolutionDir
    buildPatternDir

backendDirectRep : SifBackend
backendDirectRep = MkBackend "direct" directBuilder

-- --------------------------------------------------------------------- [ EOF ]
