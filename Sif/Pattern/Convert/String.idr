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

convertReq : REQUIREMENT impl -> String
convertReq r = unwords ["   ", show (getRTy r), ":", show (getTitle r)]

convertProblem : PROBLEM impl -> String
convertProblem p =
    unwords [ "  problem:", show (getTitle p), "\n"
            , unlines $ map (convertReq) (getReqs p)
            , "\n"]


convertAffect : AFFECT impl -> String
convertAffect a =
    unwords [ "       "
            , show (getCValue a), (getTitle $ getReq a), "\n"]

convertTrait : TRAIT impl -> String
convertTrait t =
    unwords [
        "     "
        , show (getTTy t), ":"
        , show (getTitle t)
        , "is", show (getSValue t)
        , "\n"
      , concatMap (convertAffect) (getAffects t)
      , "\n"]

convertProperty : PROPERTY impl -> String
convertProperty p =
    unwords [
        "     property: " , (getTitle p) , "\n"
     , concatMap (convertTrait) (getTraits p)
     , "\n"]


convertSolution : SOLUTION impl -> String
convertSolution s =
    unwords [
         "  solution: " , (getTitle s) , "\n"
      , concatMap convertProperty (getProperties s)
      , "\n"]

convertPattern : PATTERN impl -> String
convertPattern p =
    unwords [
         "Pattern:" , show (getTitle p) , "\n"
      , convertProblem  $ getProblem p
      , convertSolution $ getSolution p]

public
toString : PATTERN impl -> String
toString p = convertPattern p

-- --------------------------------------------------------------------- [ EOF ]
