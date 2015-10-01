-- ----------------------------------------------------------------- [ XML.idr ]
-- Module    : XML.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Convert.XML

import Effects
import Effect.State

import Data.AVL.Dict
import Data.Sigma.DList
import GRL.Lang.GLang

import XML.DOM

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API

-- -------------------------------------------------------------- [ Directives ]

%access private
%default partial

-- ------------------------------------------------------------------- [ Utils ]

mkDescNode : Maybe String -> Document ELEMENT
mkDescNode Nothing  = mkNode "description"
mkDescNode (Just d) = "description" <+=> d

mkNDNode : String -> String -> Maybe String -> Document ELEMENT
mkNDNode n t d = (mkNode n)
            <++> (mkDescNode d)
            <++> ("name" <+=> t)

-- --------------------------------------------------------------------- [ XML ]

XEffs : List EFFECT
XEffs = [STATE (Nat, Dict String Nat)]

convertReq : REQUIREMENT impl d -> Eff (Document ELEMENT) XEffs
convertReq p = do
       let ty = SifExpr.getRTy p
       let t  = SifExpr.getTitle p
       let d  = SifExpr.getDesc p
       let e  = mkNDNode (cast ty) t d
       (idGen,_) <- get
       let idVal = cast {to=Int} (S idGen)
       let e' = setAttribute "id" (cast idVal) e
       update (\(idGen,ids) => ((S idGen), insert t (S idGen) ids))
       pure $ e'

convertProblem : PROBLEM impl d -> Eff (Document ELEMENT) XEffs
convertProblem p = do
       let t  = SifExpr.getTitle p
       let d  = SifExpr.getDesc p
       let rs = SifExpr.getReqs p
       let e  = mkNode "problem"

       -- <mess>
       rsC <- mapE (\r => convertReq r) rs
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "requirements") rsC
       -- </mess>
       pure $ e
         <++> rNodes
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

convertAffect : AFFECT impl d -> Eff (Document ELEMENT) XEffs
convertAffect p = do
       let d    = SifExpr.getDesc p
       let cval = Model.SifExpr.getCValue p
       let r    = SifExpr.getReq p

       (_,ids) <- get

       let id = lookup (SifExpr.getTitle r) ids
       let e  = case d of
           Nothing => mkNode "affect"
           Just d' => "affect" <+=> d'
       let idval = cast {to=Int} $ fromMaybe 0 id

       pure $ (setAttribute "cvalue" (cast cval)
              (setAttribute "linksTo" (cast idval) e))

convertTrait : TRAIT impl d -> Eff (Document ELEMENT) XEffs
convertTrait p = do
       let ty = SifExpr.getTTy p
       let t  = SifExpr.getTitle p
       let d  = SifExpr.getDesc p
       let s  = SifExpr.getSValue p
       let as = SifExpr.getAffects p

       let e  = mkNode (toLower $ show ty)
       let e' = setAttribute "svalue" (cast s) e

       -- <mess>
       asC <- mapE (\a => convertAffect a) as
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "affects") asC
       -- </mess>
       pure $ e'
         <++> (rNodes)
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

convertProperty : PROPERTY impl d -> Eff (Document ELEMENT) XEffs
convertProperty p = do
       let t  = SifExpr.getTitle p
       let d  = SifExpr.getDesc p
       let ts = SifExpr.getTraits p
       let e  = mkNode "property"

       -- <mess>
       tsC <- mapE (\t => convertTrait t) ts
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "traits") tsC
       -- </mess>

       pure $ e
         <++> rNodes
         <++> (mkDescNode d)
         <++> ("name" <+=> t)


convertSolution : SOLUTION impl d -> Eff (Document ELEMENT) XEffs
convertSolution p = do
       let t  = SifExpr.getTitle p
       let d  = SifExpr.getDesc p
       let ps = SifExpr.getProperties p
       let e  = mkNode "solution"

       -- <mess>
       psC <- mapE (\p => convertProperty p) ps
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "properties") psC
       -- </mess>

       pure $ e
         <++> rNodes
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

convertDomain : SifDomain -> Eff (Document ELEMENT) XEffs
convertDomain (MkDomain t d) = pure $ (mkNDNode "context" t d)

convertPattern : PATTERN impl d -> Eff (Document ELEMENT) XEffs
convertPattern pat = do
       let t = SifExpr.getTitle pat
       let d = SifExpr.getDesc pat

       dNode <- convertDomain   $ SifExpr.getDomain pat
       pNode <- convertProblem  $ SifExpr.getProblem pat
       sNode <- convertSolution $ SifExpr.getSolution pat

       pure $ mkNode "sif"
          <++> sNode
          <++> pNode
          <++> dNode
          <++> (mkDescNode d)
          <++> ("name" <+=> t)

namespace Sif
  public
  toXML : PATTERN impl d -> Document DOCUMENT
  toXML p = mkDocument root
    where
      partial
      root : Document ELEMENT
      root = runPureInit [(Z,Dict.empty)] (convertPattern p)


-- --------------------------------------------------------------------- [ EOF ]
