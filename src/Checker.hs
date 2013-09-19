-- | Functions to transform the Plang AST into the formal
-- representation.
-- 
module Checker (chkPlangSpec) where

import Data.Maybe
import Data.List
import qualified Data.Map as Map  

import AST hiding (getPattern)
import TypeSystem
import Types
import Parser


chkPlangSpec :: PlangAST -> PlangSpec
chkPlangSpec spec = PlangSpec tit ps rs
    where
      tit = (title spec)
      ps = doPatterns (ps', is)
      (ps', is) = break (isJust . AST.origin) (AST.patterns spec)
      rs = doRelations (AST.relations spec) ps

doPatterns :: (PatternsExpr, PatternsExpr) -> PMap
doPatterns (ps, is) = Map.fromList $ genPatterns ps ++ getImports is

genPatterns :: PatternsExpr -> PatternValues
genPatterns ps = map genPattern ps

genPattern :: PatternExpr -> (String, Pattern)
genPattern p = (AST.ident p, newP)
               where
                 newP = Pattern (AST.name p) (modifier p) (typ p)

getImports :: PatternsExpr -> PatternValues
getImports [] = []
getImports imps = concatMap getImportGroups groups
    where
      groups = groupBy groupImports imps
      groupImports x y = AST.origin x == AST.origin y

getImportGroups :: PatternsExpr -> PatternValues
getImportGroups is = []
-- 1. Locate plang specification
-- 2. Read in spec
-- 3. Extract imported Pattern.

doRelations :: RelationsExpr -> PMap -> Relations
doRelations rs ps = map (\x -> genRelation x ps) rs

genRelation :: RelationExpr -> PMap -> Relation
genRelation r ps = case (AST.rtype r) of
                     TyAssociation -> mkAssociates x y desc
                     TySpecialisation -> mkSpecial x y desc
                     TyAggregation -> mkAggregate x y desc
                     TyRealisation -> mkRealise x y desc
                   where
                     desc = (AST.desc r)
                     x = getPattern (AST.from r) ps
                     y = getPattern (AST.to r) ps

-- --------------------------------------------------------------------- [ EOF ]
