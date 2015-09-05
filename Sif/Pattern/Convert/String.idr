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
convertReq r =
    unwords [ "   "
             , show (Pattern.getRTy r)
             , ":"
             , show (Pattern.getTitle r)]

convertProblem : PROBLEM impl d -> String
convertProblem p =
    unwords [ "  problem:", show (getTitle p), "\n"
            , unlines $ map (convertReq) (getReqs p)
            , "\n"]


convertAffect : AFFECT impl d -> String
convertAffect a =
    unwords [ "       "
            , show (getCValue a), (getTitle $ getReq a), "\n"]

convertTrait : TRAIT impl d -> String
convertTrait t =
    unwords [
        "     "
        , show (getTTy t), ":"
        , show (getTitle t)
        , "is", show (getSValue t)
        , "\n"
      , concatMap (convertAffect) (getAffects t)
      , "\n"]

convertProperty : PROPERTY impl d -> String
convertProperty p =
    unwords [
        "     property: " , (getTitle p) , "\n"
     , concatMap (convertTrait) (getTraits p)
     , "\n"]


convertSolution : SOLUTION impl d -> String
convertSolution s =
    unwords [
         "  solution: " , (getTitle s) , "\n"
      , concatMap convertProperty (getProperties s)
      , "\n"]


convertContext : SifDomain -> String
convertContext d =
    unwords [ "  context:"
            , show (getTitle d)
            , "\n"]


convertPattern : PATTERN impl d -> String
convertPattern p =
    unwords [
         "Pattern:" , show (getTitle p) , "\n"
      , convertContext  $ getDomain p
      , convertProblem  $ getProblem p
      , convertSolution $ getSolution p]

public
toString : PATTERN impl d -> String
toString p = convertPattern p

-- --------------------------------------------------------------------- [ EOF ]
