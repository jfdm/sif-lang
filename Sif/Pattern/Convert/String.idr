-- ---------------------------------------------------------------- [ Edda.idr ]
-- Module    : Edda.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Convert.String

import GRL.Lang.GLang

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API

-- -------------------------------------------------------------- [ Directives ]

%access private
%default partial

convertReq : REQUIREMENT impl d -> String
convertReq r = with SifExpr
    unwords [ "   "
             , show (getRTy r)
             , ":"
             , show (getTitle r)]

convertProblem : PROBLEM impl d -> String
convertProblem p = with SifExpr
    unwords [ "  problem:", show (getTitle p), "\n"
            , unlines $ map (convertReq) (getReqs p)
            , "\n"]


convertAffect : AFFECT impl d -> String
convertAffect a = with SifExpr
    unwords [ "       "
            , show (getCValue a), (getTitle $ getReq a), "\n"]

convertTrait : TRAIT impl d -> String
convertTrait t = with SifExpr
    unwords [
        "     "
        , show (getTTy t), ":"
        , show (getTitle t)
        , "is", show (getSValue t)
        , "\n"
      , concatMap (convertAffect) (getAffects t)
      , "\n"]

convertProperty : PROPERTY impl d -> String
convertProperty p = with SifExpr
    unwords [
        "     property: " , (getTitle p) , "\n"
     , concatMap (convertTrait) (getTraits p)
     , "\n"]


convertSolution : SOLUTION impl d -> String
convertSolution s = with SifExpr
    unwords [
         "  solution: " , (getTitle s) , "\n"
      , concatMap convertProperty (getProperties s)
      , "\n"]


convertContext : SifDomain -> String
convertContext (MkDomain t d) =
    unwords [ "  context:"
            , show t
            , "\n"]


convertPattern : PATTERN impl d -> String
convertPattern p =
    unwords [
         "Pattern:" , show (SifExpr.getTitle p) , "\n"
      , convertContext  $ SifExpr.getDomain p
      , convertProblem  $ SifExpr.getProblem p
      , convertSolution $ SifExpr.getSolution p]

namespace Sif
  export
  toString : PATTERN impl d -> String
  toString p = convertPattern p

-- --------------------------------------------------------------------- [ EOF ]
