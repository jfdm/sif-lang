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

data DirectRep : SifTy -> Type where
  DirectMkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> DirectRep tyREQ

  DirectMkProb : (title : String)
              -> (desc  : Maybe String)
              -> List (DirectRep tyREQ)
              -> DirectRep tyPROBLEM

  DirectMkAffect : (cval : CValue)
               -> (req : DirectRep tyREQ)
               -> (desc : Maybe String)
               -> DirectRep tyAFFECTS

  DirectMkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> List (DirectRep tyAFFECTS)
               -> DirectRep tyTRAIT

  DirectMkProp : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep tyTRAIT)
              -> DirectRep tyPROPERTY

  DirectMkSolt : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep tyPROPERTY)
              -> DirectRep tySOLUTION

  DirectMkPatt : (title : String)
              -> (desc : Maybe String)
              -> DirectRep tyPROBLEM
              -> DirectRep tySOLUTION
              -> DirectRep tyPATTERN

getDirectTitle : DirectRep ty -> String
getDirectTitle (DirectMkReq _ t _)       = t
getDirectTitle (DirectMkProb t _ _)      = t
getDirectTitle (DirectMkTrait _ t _ _ _) = t
getDirectTitle (DirectMkProp t _ _)      = t
getDirectTitle (DirectMkSolt t _ _)      = t
getDirectTitle (DirectMkPatt t _ _ _)    = t


getDirectDesc : DirectRep ty -> Maybe String
getDirectDesc (DirectMkReq _ _ d)       = d
getDirectDesc (DirectMkProb _ d rs)     = d
getDirectDesc (DirectMkAffect _ _ d)    = d
getDirectDesc (DirectMkTrait _ _ d _ _) = d
getDirectDesc (DirectMkProp _ d _)      = d
getDirectDesc (DirectMkSolt _ d _)      = d
getDirectDesc (DirectMkPatt _ d _ _)    = d

getDirectRTy : DirectRep tyREQ -> RTy
getDirectRTy (DirectMkReq ty _ _) = ty

getDirectTTy : DirectRep tyTRAIT -> TTy
getDirectTTy (DirectMkTrait ty _ _ _ _) = ty

getDirectSValue : DirectRep tyTRAIT -> SValue
getDirectSValue (DirectMkTrait _ _ _ sval _) = sval

getDirectCValue : DirectRep tyAFFECTS -> CValue
getDirectCValue (DirectMkAffect cval _ _) = cval

getDirectProblem : DirectRep tyPATTERN -> (DirectRep tyPROBLEM)
getDirectProblem (DirectMkPatt _ _ p _) = p

getDirectSolution : DirectRep tyPATTERN -> DirectRep tySOLUTION
getDirectSolution (DirectMkPatt _ _ _ s) = s

getDirectReqs : DirectRep tyPROBLEM -> List $ DirectRep tyREQ
getDirectReqs (DirectMkProb _ _ rs) = rs

getDirectProperties : DirectRep tySOLUTION -> List $ DirectRep tyPROPERTY
getDirectProperties (DirectMkSolt _ _ ps) = ps

getDirectTraits : DirectRep tyPROPERTY -> List $ DirectRep tyTRAIT
getDirectTraits (DirectMkProp _ _ ts) = ts

getDirectAffects : DirectRep tyTRAIT -> List $ DirectRep tyAFFECTS
getDirectAffects (DirectMkTrait _ _ _ _ as) = as

getDirectReq : DirectRep tyAFFECTS -> DirectRep tyREQ
getDirectReq (DirectMkAffect _ r _) = r

-- --------------------------------------------------------------------- [ GRL ]

covering
toGRL : DirectRep ty -> InterpRes ty
toGRL (DirectMkReq ty t d)        = interpReq t
toGRL (DirectMkProb t d rs)       = interpProb t (map toGRL rs)
toGRL (DirectMkAffect cval r d)   = interpAffect cval (toGRL r)
toGRL (DirectMkTrait ty t d s rs) = interpTrait t s (map toGRL rs) ty
toGRL (DirectMkProp t d ts)       = interpProp t (map toGRL ts)
toGRL (DirectMkSolt t d ps)       = interpSolt t (map toGRL ps)
toGRL (DirectMkPatt t d p s)      = interpPatt (toGRL p) (toGRL s)

-- ----------------------------------------------------- [ Instances and Stuff ]

getGModel : DirectRep tyPATTERN -> GModel
getGModel p = extract (toGRL p)
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m

-- ------------------------------------------------------ [ Builder Definition ]

instance SifRepAPI DirectRep where
  getTitle  x = getDirectTitle x
  getDesc   = getDirectDesc
  getTTy    = getDirectTTy
  getRTy    = getDirectRTy
  getSValue = getDirectSValue
  getCValue = getDirectCValue

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

  fetchMetaModel p = m
      where
        g : GModel
        g = getGModel p

        m : MetaModel
        m = MkModel g

-- ------------------------------------------------------------- [ The Builder ]

%inline
conv : SifExpr DirectRep ty -> DirectRep ty
conv (MkExpr x) = x

buildReqDir : RTy
           -> String
           -> Maybe String
           -> REQUIREMENT DirectRep
buildReqDir ty s d = MkExpr $ DirectMkReq ty s d

buildProblemDir : String
               -> Maybe String
               -> REQUIREMENTS DirectRep
               -> PROBLEM DirectRep
buildProblemDir t d rs =
    MkExpr $ DirectMkProb t d (map conv rs)

buildAffectDir : CValue
              -> REQUIREMENT DirectRep
              -> Maybe String
              -> AFFECT DirectRep
buildAffectDir c r d = MkExpr $ DirectMkAffect c (conv r) d

buildTraitDir : TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS DirectRep
             -> TRAIT DirectRep
buildTraitDir ty t d s rs =
    MkExpr $ DirectMkTrait ty t d s (map conv rs)


buildPropertyDir : String
                -> Maybe String
                -> TRAITS DirectRep
                -> PROPERTY DirectRep
buildPropertyDir t d ts =
    MkExpr $ DirectMkProp t d (map conv ts)

buildSolutionDir : String
                -> Maybe String
                -> PROPERTIES DirectRep
                -> SOLUTION DirectRep
buildSolutionDir s d ps =
    MkExpr $ DirectMkSolt s d (map conv ps)

buildPatternDir  : String
                -> Maybe String
                -> PROBLEM DirectRep
                -> SOLUTION DirectRep
                -> PATTERN DirectRep
buildPatternDir t d p s= MkExpr $ DirectMkPatt t d (conv p) (conv s)

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
