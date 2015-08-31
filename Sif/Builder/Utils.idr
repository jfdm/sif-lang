-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.Utils

import XML.DOM

import GRL.Lang.GLang

import Data.GraphViz.SimpleDot


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
    <++> (setAttribute "patternID" "TO BE DETERMINED" $ setAttribute "relationship" "specialises | implements | uses | linkedTo" (mkNode "link"))


grlToDot : GModel -> SimpleDot GRAPH
grlToDot g = Digraph (nodes (verticesID g)) (edges' (edges g))
  where

    f : NodeID -> String
    f n = case getValueByID n g of
        Nothing => ""
        Just u  => getNodeTitle u

    nodes : List NodeID -> List (SimpleDot NODE)
    nodes vs = map (\x => Node (x) [("label", (f x))]) vs

    convEdge : Edge GoalEdge -> SimpleDot EDGE
    convEdge (x,y, Nothing)     = Edge (y) (x) Nil
    convEdge (x,y, Just Decomp) = Edge (y) (x) [("label", "Decomp FIX")]
    convEdge (x,y, Just (Contribution l)) = Edge (x) (y) [("label", "Impacts " ++ show l)]
    convEdge (x,y, Just (Correlation  l)) = Edge (x) (y) [("label", "Affects " ++ show l)]

    edges' : List (Edge GoalEdge) -> List (SimpleDot EDGE)
    edges' es = map (convEdge) es


-- --------------------------------------------------------------------- [ EOF ]
