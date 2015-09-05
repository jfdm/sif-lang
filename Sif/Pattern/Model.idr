-- --------------------------------------------------------------- [ Model.idr ]
-- Module    : Model.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Model

import Sif.Types
import GRL.Lang.GLang

-- --------------------------------------------------- [ Public Data Structure ]

namespace Sif
  data EvalResult : Type where
    Good : List (String, Maybe SValue) -> EvalResult
    Bad  : EvalResult


||| The Meta Model API
class SifMetaModel a where
  toString : a -> String

data MetaModel : Type where
  MkModel : SifMetaModel a => a -> MetaModel

||| The Representation API
class SifRepAPI (impl : SifTy -> Type) where
    getTitle  : impl ty -> {auto prf : HasMData ty} -> String
    getDesc   : impl ty -> Maybe String
    getRTy    : impl tyREQ -> RTy
    getTTy    : impl tyTRAIT -> TTy
    getSValue : impl tyTRAIT -> SValue
    getCValue : impl tyAFFECTS -> CValue

    getProblem    : impl tyPATTERN -> impl tyPROBLEM
    getSolution   : impl tyPATTERN -> impl tySOLUTION
    getReqs       : impl tyPROBLEM -> List (impl tyREQ)
    getProperties : impl tySOLUTION -> List (impl tyPROPERTY)
    getTraits     : impl tyPROPERTY -> List (impl tyTRAIT)
    getAffects    : impl tyTRAIT -> List (impl tyAFFECTS)
    getReq        : impl tyAFFECTS -> impl tyREQ

    evalPattern    : impl tyPATTERN -> Sif.EvalResult
    fetchMetaModel : impl tyPATTERN -> MetaModel


instance SifMetaModel MetaModel where
  toString (MkModel m) = toString m

data SifExpr : (impl : SifTy -> Type) -> SifTy -> Type where
  MkExpr : SifRepAPI impl => impl ty -> SifExpr impl ty

instance SifRepAPI (\ty => SifExpr l ty) where
  getTitle      (MkExpr x) = getTitle x
  getDesc       (MkExpr x) = getDesc x
  getTTy        (MkExpr x) = getTTy x
  getRTy        (MkExpr x) = getRTy x
  getSValue     (MkExpr x) = getSValue x
  getCValue     (MkExpr x) = getCValue x

  getProblem    (MkExpr x) = MkExpr $ getProblem x
  getSolution   (MkExpr x) = MkExpr $ getSolution x
  getReqs       (MkExpr x) = map MkExpr $ getReqs x
  getProperties (MkExpr x) = map MkExpr $ getProperties x
  getTraits     (MkExpr x) = map MkExpr $ getTraits x
  getAffects    (MkExpr x) = map MkExpr $ getAffects x
  getReq        (MkExpr x) = MkExpr $ getReq x

  evalPattern (MkExpr x)    = evalPattern x
  fetchMetaModel (MkExpr x) = fetchMetaModel x

||| Factories for building concrete representations
record SifBuilder (impl : SifTy -> Type) where
  constructor MkSifBuilder
  buildReq : RTy
          -> String
          -> Maybe String
          -> SifExpr impl tyREQ

  buildProblem : String
              -> Maybe String
              -> List (SifExpr impl tyREQ)
              -> SifExpr impl tyPROBLEM

  buildAffect : CValue
             -> SifExpr impl tyREQ
             -> Maybe String
             -> SifExpr impl tyAFFECTS

  buildTrait : TTy
            -> String
            -> Maybe String
            -> SValue
            -> List $ SifExpr impl tyAFFECTS
            -> SifExpr impl tyTRAIT

  buildProperty : String
               -> Maybe String
               -> List $ SifExpr impl tyTRAIT
               -> SifExpr impl tyPROPERTY

  buildSolution : String
               -> Maybe String
               -> List $ SifExpr impl tyPROPERTY
               -> SifExpr impl tySOLUTION

  buildPattern  : String
               -> Maybe String
               -> SifExpr impl tyPROBLEM
               -> SifExpr impl tySOLUTION
               -> SifExpr impl tyPATTERN

record SifBackend where
    constructor MkBackend
    name    : String
    builder : (SifBuilder impl)

-- --------------------------------------------------------------------- [ EOF ]
