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

namespace Domain
  record SifDomain where
    constructor MkDomain
    getTitle : String
    getDesc  : Maybe String

data MetaModel : Type where
  MkModel : SifMetaModel a => a -> MetaModel

namespace Pattern

  ||| The Representation API
  class SifRepAPI (impl : SifTy -> SifDomain -> Type) where
      getTitle  : impl ty d -> {auto prf : HasMData ty} -> String
      getDesc   : impl ty d -> Maybe String
      getRTy    : impl tyREQ     d -> RTy
      getTTy    : impl tyTRAIT   d -> TTy
      getSValue : impl tyTRAIT   d -> SValue
      getCValue : impl tyAFFECTS d -> CValue

      getProblem  : impl tyPATTERN d -> impl tyPROBLEM  d
      getSolution : impl tyPATTERN d -> impl tySOLUTION d

      getReqs       : impl tyPROBLEM  d -> List (impl tyREQ      d)
      getProperties : impl tySOLUTION d -> List (impl tyPROPERTY d)
      getTraits     : impl tyPROPERTY d -> List (impl tyTRAIT    d)
      getAffects    : impl tyTRAIT    d -> List (impl tyAFFECTS  d)
      getReq        : impl tyAFFECTS  d -> impl tyREQ d

      evalPattern    : impl tyPATTERN d -> Sif.EvalResult
      fetchMetaModel : impl tyPATTERN d -> MetaModel

      getDomain : impl ty d -> SifDomain
      getDomain {d} _ = d

instance SifMetaModel MetaModel where
  toString (MkModel m) = toString m

data SifExpr : SifTy
            -> SifDomain
            -> (impl : SifTy -> SifDomain -> Type)
            -> Type
  where
    MkExpr : SifRepAPI impl => impl ty d -> SifExpr ty d impl

instance SifRepAPI (\ty,d => SifExpr ty d impl) where
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
record SifBuilder (impl : SifTy -> SifDomain -> Type) (d : SifDomain) where
  constructor MkSifBuilder

  buildReq : RTy
          -> String
          -> Maybe String
          -> SifExpr tyREQ d impl

  buildProblem : String
              -> Maybe String
              -> List (SifExpr tyREQ d impl)
              -> SifExpr tyPROBLEM d impl

  buildAffect : CValue
             -> SifExpr tyREQ d impl
             -> Maybe String
             -> SifExpr tyAFFECTS d impl

  buildTrait : TTy
            -> String
            -> Maybe String
            -> SValue
            -> List $ SifExpr tyAFFECTS d impl
            -> SifExpr tyTRAIT d impl

  buildProperty : String
               -> Maybe String
               -> List $ SifExpr tyTRAIT d impl
               -> SifExpr tyPROPERTY d impl

  buildSolution : String
               -> Maybe String
               -> List $ SifExpr tyPROPERTY d impl
               -> SifExpr tySOLUTION d impl

  buildPattern  : String
               -> Maybe String
               -> SifExpr tyPROBLEM  d impl
               -> SifExpr tySOLUTION d impl
               -> SifExpr tyPATTERN  d impl

record SifBackend where
    constructor MkBackend
    name    : String
    builder : (SifBuilder impl d)

-- --------------------------------------------------------------------- [ EOF ]
