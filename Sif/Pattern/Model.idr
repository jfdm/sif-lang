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

namespace Pattern

  ||| The Representation API
  class SifRepAPI (impl : SifTy -> SifDomain -> Type) where
      getTitle  : impl ty d -> {auto prf : HasMData ty} -> String
      getDesc   : impl ty d -> Maybe String
      getRTy    : impl TyREQ     d -> RTy
      getTTy    : impl TyTRAIT   d -> TTy
      getSValue : impl TyTRAIT   d -> SValue
      getCValue : impl TyAFFECTS d -> CValue

      getProblem  : impl TyPATTERN d -> impl TyPROBLEM  d
      getSolution : impl TyPATTERN d -> impl TySOLUTION d

      getReqs       : impl TyPROBLEM  d -> List (impl TyREQ      d)
      getProperties : impl TySOLUTION d -> List (impl TyPROPERTY d)
      getTraits     : impl TyPROPERTY d -> List (impl TyTRAIT    d)
      getAffects    : impl TyTRAIT    d -> List (impl TyAFFECTS  d)
      getReq        : impl TyAFFECTS  d -> impl TyREQ d

      evalPattern    : impl TyPATTERN d -> Sif.EvalResult
      fetchMetaModel : impl TyPATTERN d -> MetaModel

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

  getRTy : SifExpr TyREQ d impl -> RTy
  getRTy (MkExpr x) = Pattern.getRTy x

  getTTy : SifExpr TyTRAIT d impl -> TTy
  getTTy (MkExpr x) = Pattern.getTTy x

  getSValue : SifExpr TyTRAIT d impl -> SValue
  getSValue (MkExpr x) = Pattern.getSValue x

  getCValue : SifExpr TyAFFECTS d impl -> CValue
  getCValue (MkExpr x) = Pattern.getCValue x

  getProblem : SifExpr TyPATTERN d impl -> SifExpr TyPROBLEM d impl
  getProblem (MkExpr x) = MkExpr $ Pattern.getProblem x

  getSolution : SifExpr TyPATTERN d impl -> SifExpr TySOLUTION d impl
  getSolution (MkExpr x) = MkExpr $ Pattern.getSolution x

  getReqs : SifExpr TyPROBLEM d impl -> List (SifExpr TyREQ d impl)
  getReqs (MkExpr x) = map MkExpr $ Pattern.getReqs x

  getProperties : SifExpr TySOLUTION d impl -> List (SifExpr TyPROPERTY d impl)
  getProperties (MkExpr x) = map MkExpr $ Pattern.getProperties x

  getTraits : SifExpr TyPROPERTY d impl -> List (SifExpr TyTRAIT d impl)
  getTraits (MkExpr x) = map MkExpr $ Pattern.getTraits x

  getAffects : SifExpr TyTRAIT d impl -> List (SifExpr TyAFFECTS d impl)
  getAffects (MkExpr x) = map MkExpr $ Pattern.getAffects x

  getReq : SifExpr TyAFFECTS d impl -> SifExpr TyREQ d impl
  getReq (MkExpr x) = MkExpr $ Pattern.getReq x

  getDomain : SifExpr ty d impl -> SifDomain
  getDomain {d} _ = d

  evalPattern : SifExpr TyPATTERN d impl -> Sif.EvalResult
  evalPattern (MkExpr x)    = Pattern.evalPattern x

  fetchMetaModel : SifExpr TyPATTERN d impl -> MetaModel
  fetchMetaModel (MkExpr x) = Pattern.fetchMetaModel x

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
          -> SifExpr TyREQ d impl

  buildProblem : (d : SifDomain)
              -> String
              -> Maybe String
              -> List (SifExpr TyREQ d impl)
              -> SifExpr TyPROBLEM d impl

  buildAffect : (d : SifDomain)
             -> CValue
             -> SifExpr TyREQ d impl
             -> Maybe String
             -> SifExpr TyAFFECTS d impl

  buildTrait : (d : SifDomain)
            -> TTy
            -> String
            -> Maybe String
            -> SValue
            -> List $ SifExpr TyAFFECTS d impl
            -> SifExpr TyTRAIT d impl

  buildProperty : (d : SifDomain)
               -> String
               -> Maybe String
               -> List $ SifExpr TyTRAIT d impl
               -> SifExpr TyPROPERTY d impl

  buildSolution : (d : SifDomain)
               -> String
               -> Maybe String
               -> List $ SifExpr TyPROPERTY d impl
               -> SifExpr TySOLUTION d impl

  buildPattern : (d : SifDomain)
              -> String
              -> Maybe String
              -> SifExpr TyPROBLEM  d impl
              -> SifExpr TySOLUTION d impl
              -> SifExpr TyPATTERN  d impl

record SifBackend where
    constructor MkBackend
    name    : String
    builder : (SifBuilder impl)

-- --------------------------------------------------------------------- [ EOF ]
