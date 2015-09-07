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

  defaultDomain : SifDomain
  defaultDomain = MkDomain "Default" (Just "Not Specified")

  instance Eq SifDomain where
    (==) (MkDomain x xd) (MkDomain y yd) = x == y && xd == yd

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

namespace SifExpr
  getTitle : SifExpr ty d impl -> {auto prf : HasMData ty} -> String
  getTitle (MkExpr x) = Pattern.getTitle x

  getDesc : SifExpr ty d impl -> Maybe String
  getDesc (MkExpr x) = Pattern.getDesc x

  getRTy : SifExpr tyREQ d impl -> RTy
  getRTy (MkExpr x) = Pattern.getRTy x

  getTTy : SifExpr tyTRAIT d impl -> TTy
  getTTy (MkExpr x) = Pattern.getTTy x

  getSValue : SifExpr tyTRAIT d impl -> SValue
  getSValue (MkExpr x) = Pattern.getSValue x

  getCValue : SifExpr tyAFFECTS d impl -> CValue
  getCValue (MkExpr x) = Pattern.getCValue x

  getProblem : SifExpr tyPATTERN d impl -> SifExpr tyPROBLEM d impl
  getProblem (MkExpr x) = MkExpr $ Pattern.getProblem x

  getSolution : SifExpr tyPATTERN d impl -> SifExpr tySOLUTION d impl
  getSolution (MkExpr x) = MkExpr $ Pattern.getSolution x

  getReqs : SifExpr tyPROBLEM d impl -> List (SifExpr tyREQ d impl)
  getReqs (MkExpr x) = map MkExpr $ Pattern.getReqs x

  getProperties : SifExpr tySOLUTION d impl -> List (SifExpr tyPROPERTY d impl)
  getProperties (MkExpr x) = map MkExpr $ Pattern.getProperties x

  getTraits : SifExpr tyPROPERTY d impl -> List (SifExpr tyTRAIT d impl)
  getTraits (MkExpr x) = map MkExpr $ Pattern.getTraits x

  getAffects : SifExpr tyTRAIT d impl -> List (SifExpr tyAFFECTS d impl)
  getAffects (MkExpr x) = map MkExpr $ Pattern.getAffects x

  getReq : SifExpr tyAFFECTS d impl -> SifExpr tyREQ d impl
  getReq (MkExpr x) = MkExpr $ Pattern.getReq x

  getDomain : SifExpr ty d impl -> SifDomain
  getDomain {d} _ = d

  evalPattern : SifExpr tyPATTERN d impl -> Sif.EvalResult
  evalPattern (MkExpr x)    = Pattern.evalPattern x

  fetchMetaModel : SifExpr tyPATTERN d impl -> MetaModel
  fetchMetaModel (MkExpr x) = Pattern.fetchMetaModel x

-- Causing too mushc problems than it is worth...
-- instance SifRepAPI (\ty,d => SifExpr ty d impl) where
--   getTitle   x = SifExpr.getTitle x
--   getDesc    x = SifExpr.getDesc x
--   getTTy     x = SifExpr.getTTy x
--   getRTy     x = SifExpr.getRTy x
--   getSValue  x = SifExpr.getSValue x
--   getCValue  x = SifExpr.getCValue x

--   getProblem    x = SifExpr.getProblem x
--   getSolution   x = SifExpr.getSolution x
--   getReqs       x = SifExpr.getReqs x
--   getProperties x = SifExpr.getProperties x
--   getTraits     x = SifExpr.getTraits x
--   getAffects    x = SifExpr.getAffects x
--   getReq        x = SifExpr.getReq x

--   evalPattern (MkExpr x)    = evalPattern x
--   fetchMetaModel (MkExpr x) = fetchMetaModel x

-- Better to parameterise SifBuilder by a SifDomain. the problem is
-- not how to populate the value in the type---a update function that
-- populates a record field and type value at the sametime. Rather the
-- problem is how to have a clean api when defining instances of
-- this data type in later code sans dependent pairs.

||| Factories for building concrete representations
|||
record SifBuilder (impl : SifTy -> SifDomain -> Type) where
  constructor MkSifBuilder
  buildReq : (d : SifDomain)
          -> RTy
          -> String
          -> Maybe String
          -> SifExpr tyREQ d impl

  buildProblem : (d : SifDomain)
              -> String
              -> Maybe String
              -> List (SifExpr tyREQ d impl)
              -> SifExpr tyPROBLEM d impl

  buildAffect : (d : SifDomain)
             -> CValue
             -> SifExpr tyREQ d impl
             -> Maybe String
             -> SifExpr tyAFFECTS d impl

  buildTrait : (d : SifDomain)
            -> TTy
            -> String
            -> Maybe String
            -> SValue
            -> List $ SifExpr tyAFFECTS d impl
            -> SifExpr tyTRAIT d impl

  buildProperty : (d : SifDomain)
               -> String
               -> Maybe String
               -> List $ SifExpr tyTRAIT d impl
               -> SifExpr tyPROPERTY d impl

  buildSolution : (d : SifDomain)
               -> String
               -> Maybe String
               -> List $ SifExpr tyPROPERTY d impl
               -> SifExpr tySOLUTION d impl

  buildPattern : (d : SifDomain)
              -> String
              -> Maybe String
              -> SifExpr tyPROBLEM  d impl
              -> SifExpr tySOLUTION d impl
              -> SifExpr tyPATTERN  d impl

record SifBackend where
    constructor MkBackend
    name    : String
    builder : (SifBuilder impl)

-- --------------------------------------------------------------------- [ EOF ]
