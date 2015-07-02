-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Pattern.Utils

import XML.DOM

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
    <++> mkDescNode Nothing
    <++> mkModel

mkDynamics : Document ELEMENT
mkDynamics = addScore $ mkNode "dynamics"
    <++> mkDescNode Nothing
    <++> mkModel

mkRels : Document ELEMENT
mkRels = addScore $ mkNode "relations"
    <++> (setAttribute "patternID" "TO BE DETERMINED" $ setAttribute "relationship" "specialises | implements | uses | linkedTo" (mkNode "link"))

-- --------------------------------------------------------------------- [ EOF ]
