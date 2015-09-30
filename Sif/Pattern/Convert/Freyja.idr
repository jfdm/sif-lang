-- -------------------------------------------------------------- [ Freyja.idr ]
-- Module    : Freyja.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Convert.Freyja

import Data.AVL.Dict
import Data.Sigma.DList
import GRL.Lang.GLang

import XML.DOM

import Edda
import Edda.Reader.Org

import Freyja

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API

%access private

-- ------------------------------------------------------------------- [ Utils ]

inlines : String -> EddaString
inlines s =
  case readOrgInline s of
    Left err  => [Text "Error"]
    Right res => res

paras : Maybe String -> EddaBody
paras Nothing  = [Para (inlines "To Added")]
paras (Just s) =
  case readOrgBody s of
    Left err  => [Para (inlines $ show err)]
    Right res => res

-- ------------------------------------------------------------ [ Requirements ]

convertReq : REQUIREMENT impl d -> Sigma RTy Requirement
convertReq r = (ty ** MkReq ty n d)
  where
    n : EddaString
    n = inlines (SifExpr.getTitle r)

    d : EddaBody
    d = paras (SifExpr.getDesc r)

    ty : RTy
    ty = SifExpr.getRTy r

convertProblem : PROBLEM impl d -> Problem
convertProblem p = MkProblem n d (getProof rs)
  where
    n : EddaString
    n = inlines (SifExpr.getTitle p)

    d : EddaBody
    d = paras (SifExpr.getDesc p)

    rs : (rs' ** DList RTy Requirement rs')
    rs = fromLDP $ map (convertReq) (SifExpr.getReqs p)

-- ---------------------------------------------------------------- [ Solution ]


convertAffect : AFFECT impl d -> Affect
convertAffect a = MkAffect c (getProof r) d
  where
    c : CValue
    c = SifExpr.getCValue a

    r : (Sigma RTy Requirement)
    r = convertReq $ SifExpr.getReq a

    d : Maybe EddaBody
    d = case (SifExpr.getDesc a) of
          Nothing => Nothing
          d'      => Just $ paras d'

-- ------------------------------------------------------------------- [ Trait ]

convertTrait : TRAIT impl d -> Sigma TTy Trait
convertTrait t = (ty ** MkTrait ty n d (SifExpr.getSValue t) as)
  where
    ty : TTy
    ty = (SifExpr.getTTy t)

    n : EddaString
    n = inlines (SifExpr.getTitle t)

    d : EddaBody
    d = paras (SifExpr.getDesc t)

    as : List Affect
    as = map (convertAffect) $ SifExpr.getAffects t


-- ---------------------------------------------------------------- [ Property ]

convertProperty : PROPERTY impl d -> Property
convertProperty p = MkProperty t d (getProof ts)
  where
    t : EddaString
    t = inlines (SifExpr.getTitle p)

    d : EddaBody
    d = paras (SifExpr.getDesc p)

    ts : (ls ** DList TTy Trait ls)
    ts = fromLDP $ map convertTrait (SifExpr.getTraits p)

-- ---------------------------------------------------------------- [ Solution ]

convertSolution : SOLUTION impl d -> Solution
convertSolution s = MkSolution t d [ms,md] ps
  where
    t : EddaString
    t = inlines (SifExpr.getTitle s)

    d : EddaBody
    d = paras (SifExpr.getDesc s)

    ms : Model STRUCT
    ms = MkModel (inlines "Example Structure")
                 STRUCT
                 (paras Nothing)
                 "MODEL inserted here"

    md : Model DYN
    md = MkModel (inlines "Example Dynamic")
                 DYN
                 (paras Nothing)
                 "MODEL inserted here"

    ps : List Property
    ps = map (convertProperty) $ SifExpr.getProperties s

-- ------------------------------------------------------------------ [ Domain ]

convertDomain : SifDomain -> Context
convertDomain (MkDomain t d) = MkContext (inlines t) (paras d)

-- ----------------------------------------------------------------- [ Pattern ]

convertPattern : PATTERN impl d -> PatternDoc
convertPattern p = MkPDoc n d md c p' s e ss Nil
  where
    n : EddaString
    n = inlines (SifExpr.getTitle p)

    d : EddaBody
    d = paras $ SifExpr.getDesc p

    md : Metadata
    md = MkMData Nil Nil Nil Nil "nout" "nout" "nout" "nout"

    c : Context
    c = convertDomain (SifExpr.getDomain p)

    p' : Problem
    p' = convertProblem $ SifExpr.getProblem p

    s : Solution
    s = convertSolution $ SifExpr.getSolution p

    e : EddaBody
    e = paras Nothing

    ss : List Study
    ss = [MkStudy (inlines "To be determined") e e]

public
toFreyja : PATTERN impl d -> PatternDoc
toFreyja p = convertPattern p
