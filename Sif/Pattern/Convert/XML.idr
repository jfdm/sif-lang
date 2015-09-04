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

mkNode : String -> Document ELEMENT
mkNode = mkSimpleElement

mkPCNode : String -> String -> Document ELEMENT
mkPCNode p c = mkNode p <++> (c <+=> "TO BE DETERMIND")

mkEmptyNode : String -> Document ELEMENT
mkEmptyNode s = (s <+=> "TO BE DETERMIND")

addScore : Document ELEMENT -> Document ELEMENT
addScore = setAttribute "score" "TO BE DETERMINED"

mkDescNode : Maybe String -> Document ELEMENT
mkDescNode Nothing  = "description" <+=> "TO BE DETERMINED"
mkDescNode (Just d) = "description" <+=> d

mkNDNode : String -> String -> Maybe String -> Document ELEMENT
mkNDNode n t d = (addScore $ mkNode n)
            <++> (addScore $ mkDescNode d)
            <++> (addScore $ "name" <+=> t)

--addND : Document ELEMENT -> String -> Maybe String -> Document ELEMENT
--addND n t d =

mkMdata : Document ELEMENT
mkMdata = mkNode "metadata"
     <++> mkPCNode "auditors" "auditor"
     <++> mkPCNode "authors" "author"
     <++> mkEmptyNode "evaluated"
     <++> mkEmptyNode "modified"
     <++> mkEmptyNode "created"
     <++> mkPCNode "tags" "tag"
     <++> mkPCNode "aliases" "alias"

mkStudies : Document ELEMENT
mkStudies = addScore $ mkNode "studies"
    <++> (addScore $ (mkNode "study"
          <++> mkNode "after"
          <++> mkNode "before"))

mkModel : Document ELEMENT
mkModel = setAttribute
    "modelTy"
    "class | component | sequence | deployment" $
    ((mkNode "model") <++> mkDescNode Nothing <++> CData "TO BE DETERMINED")

mkStructure : Document ELEMENT
mkStructure = addScore $ mkNode "structure"
    <++> (addScore $ mkDescNode Nothing)
    <++> mkModel

mkDynamics : Document ELEMENT
mkDynamics = addScore $ mkNode "dynamics"
    <++> (addScore $ mkDescNode Nothing)
    <++> mkModel

mkRels : Document ELEMENT
mkRels = addScore $ mkNode "relations"
    <++> (setAttribute "patternID" "TO BE DETERMINED" $
            setAttribute "relationship" "specialises | implements | uses | linkedTo" (mkNode "link"))


-- --------------------------------------------------------------------- [ XML ]

XEffs : List EFFECT
XEffs = [STATE (Nat, Dict String Nat)]

convertReq : REQUIREMENT impl -> Eff (Document ELEMENT) XEffs
convertReq p = do
       let ty = getRTy p
       let t = getTitle p
       let d = getDesc p
       let e  = addScore $ mkNDNode (cast ty) t d
       (idGen,_) <- get
       let idVal = cast {to=Int} (S idGen)
       let e' = setAttribute "id" (cast idVal) e
       update (\(idGen,ids) => ((S idGen), insert t (S idGen) ids))
       pure $ e'

convertProblem : PROBLEM impl -> Eff (Document ELEMENT) XEffs
convertProblem p = do
       let t = getTitle p
       let d = getDesc p
       let rs = getReqs p
       let e = addScore $ mkNode "problem"
       -- The following mess is because I could not get Effectful HOFs for DList to work.
       -- <mess>
       rsC <- mapE (\r => convertReq r) rs
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "requirements") rsC
       -- </mess>
       pure $ e
         <++> rNodes
         <++> (addScore $ mkDescNode d)
         <++> ("name" <+=> t)

convertAffect : AFFECT impl -> Eff (Document ELEMENT) XEffs
convertAffect p = do
       let d = getDesc p
       let cval = Model.getCValue p
       let r = getReq p
       (_,ids) <- get
       let id = lookup (getTitle r) ids
       let e = case d of
           Nothing => mkNode "affect"
           Just d' => "affect" <+=> d'
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ (setAttribute "cvalue" (cast cval)
              (setAttribute "linksTo" (cast idval) e))

convertTrait : TRAIT impl -> Eff (Document ELEMENT) XEffs
convertTrait p = do
       let ty = getTTy p
       let t = getTitle p
       let d = getDesc p
       let s = getSValue p
       let as = getAffects p
       let e  = addScore $ mkNode (toLower $ show ty)
       let e' = setAttribute "svalue" (cast s) e
       -- <mess>
       asC <- mapE (\a => convertAffect a) as
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "affects") asC
       -- </mess>
       pure $ e'
         <++> (rNodes)
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

convertProperty : PROPERTY impl -> Eff (Document ELEMENT) XEffs
convertProperty p = do
       let t = getTitle p
       let d = getDesc p
       let ts = getTraits p
       let e = addScore $ mkNode "property"
       -- <mess>
       tsC <- mapE (\t => convertTrait t) ts
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "traits") tsC
       -- </mess>
       pure $ e
         <++> rNodes
         <++> (mkDescNode d)
         <++> ("name" <+=> t)


convertSolution : SOLUTION impl -> Eff (Document ELEMENT) XEffs
convertSolution p = do
       let t = getTitle p
       let d = getDesc p
       let ps = getProperties p
       let e = addScore $ mkNode "solution"
       -- <mess>
       psC <- mapE (\p => convertProperty p) ps
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "properties") psC
       -- </mess>
       pure $ e
         <++> (addScore rNodes)
         <++> mkStructure
         <++> mkDynamics
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

convertPattern : PATTERN impl -> Eff (Document ELEMENT) XEffs
convertPattern pat = do
       let t = getTitle pat
       let d = getDesc pat
       let p = getProblem pat
       let s = getSolution pat

       let e = mkNode "pattern"
       pNode <- convertProblem p
       sNode <- convertSolution s
       pure $ e <++> mkRels
                <++> mkStudies
                <++> (addScore $ mkEmptyNode "evidence")
                <++> sNode
                <++> pNode
                <++> (addScore $ mkEmptyNode "context")
                <++> mkMdata
                <++> (mkDescNode d)
                <++> ("name" <+=> t)

public
toXML : PATTERN impl -> Document DOCUMENT
toXML p = setRoot root $ mkDocument (mkQName "pattern") Nothing
  where
    partial
    root : Document ELEMENT
    root = runPureInit [(Z,Dict.empty)] (convertPattern p)


-- --------------------------------------------------------------------- [ EOF ]
