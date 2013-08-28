module Checker where

import System.IO
import Data.List
import Data.Maybe
import Parser
import Model.AST
import Examples.AST

-- ------------------------------------------------------ [ Checking Functions ]

getPatternIDs :: Plang -> IDs 
getPatternIDs plang = map Model.ident (Model.patterns plang)

getImportIDs :: Plang -> IDs
getImportIDs plang = mapMaybe Model.pattern (fromJust (Model.imports plang))

getInstRelationIDs :: Plang -> IDs
getInstRelationIDs plang = nub $ union tos froms
                           where
                             tos = map Model.from (Model.relations plang)
                             froms = concatMap Model.to (Model.relations plang)



getClassRelationIDs :: Plang -> IDs
getClassRelationIDs plang = nub $ union extends implements
                            where
                              extends = concat $ mapMaybe Model.extends (Model.patterns plang)
                              implements = concat $ mapMaybe Model.implements (Model.patterns plang)

-- Checks for orphan patterns
-- Returns the list pf orphan pattern identifiers
orphanIDs :: Plang -> IDs
orphanIDs plang = p \\ r
                  where
                    p = case Model.imports plang of
                          Nothing -> getPatternIDs plang
                          otherwise -> getPatternIDs plang `union` getImportIDs plang
                    r = getInstRelationIDs plang `union` getClassRelationIDs plang


