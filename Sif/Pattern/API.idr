-- ----------------------------------------------------------------- [ API.idr ]
-- Module    : API.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Pattern.API

import Effects
import Effect.State

import Data.Sigma.DList
import Data.AVL.Dict
import Data.GraphViz.SimpleDot

import GRL.Lang.GLang
import GRL.Eval

import Edda
import Edda.Reader.Org

import XML.DOM

import Sif.Pattern.Model
import Sif.Pattern.Utils

%default partial
%access public

-- --------------------------------------------------- [ Public Data Structure ]

abstract
data SifExpr : SifTy -> Type where
  MkExpr  : {i : InterpRes ty} -> SifPriv i ty-> SifExpr ty


private
partial
showSifExpr : SifExpr ty -> String
showSifExpr (MkExpr e) = toOrg e

instance Show (SifExpr ty) where
  show x = showSifExpr x

-- ----------------------------------------------------------- [ Type Synonyms ]

FUNCTIONAL : Type
FUNCTIONAL = SifExpr tyREQ

USABILITY : Type
USABILITY = SifExpr tyREQ

RELIABILITY : Type
RELIABILITY = SifExpr tyREQ

PERFORMANCE : Type
PERFORMANCE = SifExpr tyREQ

SUPPORTABILITY : Type
SUPPORTABILITY = SifExpr tyREQ

REQUIREMENT : Type
REQUIREMENT = SifExpr tyREQ

REQUIREMENTS : Type
REQUIREMENTS = List (SifExpr tyREQ)

PROBLEM : Type
PROBLEM = SifExpr tyPROBLEM

ADVANTAGE : Type
ADVANTAGE = SifExpr tyTRAIT

DISADVANTAGE : Type
DISADVANTAGE = SifExpr tyTRAIT

TRAIT : Type
TRAIT = SifExpr tyTRAIT

TRAITS : Type
TRAITS = List (SifExpr tyTRAIT)

TLINK : Type
TLINK = SifExpr tyTRAITend

TLINKS : Type
TLINKS = List (SifExpr tyTRAITend)

PROPERTY : Type
PROPERTY = SifExpr tyPROPERTY

PROPERTIES : Type
PROPERTIES = List (SifExpr tyPROPERTY)

SOLUTION : Type
SOLUTION = SifExpr tySOLUTION

PATTERN : Type
PATTERN = SifExpr tyPATTERN

-- Public Constructors

private
conv : List (SifExpr ty)
    -> (xs ** DList (InterpRes ty) (\x => SifPriv x ty) xs)
conv xs = fromLDP $ map (\(MkExpr x) => (_ ** x)) xs

mkFunctional : String -> Maybe String -> FUNCTIONAL
mkFunctional s desc = MkExpr $ priv__mkReq FUNC s desc

mkUsability : String -> Maybe String-> USABILITY
mkUsability s desc = MkExpr $ priv__mkReq USAB s desc

mkReliability : String -> Maybe String -> RELIABILITY
mkReliability s desc = MkExpr $ priv__mkReq RELI s desc

mkPerformance : String -> Maybe String -> PERFORMANCE
mkPerformance s desc = MkExpr $ priv__mkReq PERF s desc

mkSupportability : String -> Maybe String -> SUPPORTABILITY
mkSupportability s desc = MkExpr $ priv__mkReq SUPP s desc

private
convR : SifExpr tyREQ -> (r : InterpRes tyREQ ** SifPriv r tyREQ)
convR (MkExpr res) = (_ ** res)

partial
mkProblem : String
         -> Maybe String
         -> (ls : List (SifExpr tyREQ))
  --       -> {auto prf : NonEmpty ls}
         -> PROBLEM
mkProblem s d rs = MkExpr $ priv__mkProb s d (Sigma.getProof $ conv rs)

mkLink : CValue -> REQUIREMENT -> TLINK
mkLink c r = MkExpr $ priv__mkTLink c (Sigma.getProof $ convR r)

mkAdvantage : String
           -> Maybe String
           -> SValue
           -> (ts : TLINKS)
 --          -> {auto prf : NonEmpty ts}
           -> ADVANTAGE
mkAdvantage t d s rs =
    MkExpr $ priv__mkTrait ADV t d s (Sigma.getProof $ conv rs)

mkDisadvantage : String
              -> Maybe String
              -> SValue
              -> (ts : TLINKS)
--              -> {auto prf : NonEmpty ts}
              -> DISADVANTAGE
mkDisadvantage t d s rs =
    MkExpr $ priv__mkTrait DIS t d s (Sigma.getProof $ conv rs)

mkProperty : String
          -> Maybe String
          -> (ts : TRAITS)
--          -> {auto prf : NonEmpty ts}
          -> PROPERTY
mkProperty t d ts = MkExpr $ priv__mkProp t d (Sigma.getProof $ conv ts)

mkSolution : String
          -> Maybe String
          -> (ps : PROPERTIES)
 --         -> {auto prf : NonEmpty ps}
          -> SOLUTION
mkSolution s d ps = MkExpr $ priv__mkSolt s d (Sigma.getProof $ conv ps)

mkPattern : String -> Maybe String -> PROBLEM -> SOLUTION -> PATTERN
mkPattern t d (MkExpr p) (MkExpr s) = MkExpr $ priv__mkPatt t d p s

getModel : PATTERN -> GModel
getModel (MkExpr p) = doGetModel p
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m

    doGetModel : {i : InterpRes tyPATTERN} -> SifPriv i tyPATTERN -> GModel
    doGetModel {i} _ = extract i

covering
evalPattern : PATTERN -> EvalResult
evalPattern p = evalModel (getModel p) Nothing

getPatternTitle : PATTERN -> String
getPatternTitle (MkExpr p) = doGet p
  where
    doGet : {i : InterpRes tyPATTERN} -> SifPriv i tyPATTERN -> String
    doGet (priv__mkPatt t _ _ _) = t

-- ----------------------------------------------------------------- [ To Edda ]

partial
toEdda : SifExpr tyPATTERN -> Maybe $ Edda PRIME MODEL
toEdda expr =
    case parse parseOrg (showSifExpr expr) of
      Left  err => Nothing
      Right doc => Just (refineEdda doc)

partial
toXML : SifExpr tyPATTERN -> Document DOCUMENT
toXML (MkExpr x) = setRoot root $ mkDocument (mkQName "pattern") Nothing
  where
    partial
    root : Document ELEMENT
    root = runPureInit [(Z,Dict.empty)] (Model.toXML x)

toDot : SifExpr tyPATTERN -> SimpleDot GRAPH
toDot p = grlToDot $ getModel p

-- --------------------------------------------------------------------- [ EOF ]
