-- | Functions to transform the Plang AST into the formal
-- representation.
-- 
module Checker (chkPlangSpec) where

import Data.Maybe
import Data.List

import AST hiding (getPattern)
import Model
import Types
import Parser


chkPlangSpec :: PlangAST -> PlangSpec
chkPlangSpec spec = PlangSpec tit lab is ps rs
    where
      tit = (AST.title spec)
      lab = (AST.label spec)
      ps = doPatterns ps'
      is = doImports is'
      (ps', is') = break (isJust . AST.origin) (AST.patterns spec)
      rs = doRelations (AST.relations spec) (ps ++ is)

-- ----------------------------------------------------------------- [ Imports ]
doImports :: PatternsExpr -> Patterns
doImports [] = []
doImports is = concatMap getImportGroups groups
    where
      groups = groupBy groupImports is
      groupImports x y = AST.origin x == AST.origin y

-- @TODO
getImportGroups :: PatternsExpr -> Patterns
getImportGroups is = []
-- 1. Locate plang specification
-- 2. Read in spec
-- 3. Extract imported Pattern.

-- ---------------------------------------------------------------- [ Patterns ]

doPatterns :: PatternsExpr -> Patterns
doPatterns ps = map genPattern ps

genPattern :: PatternExpr -> PatternItem
genPattern p = (id, newP)
               where
                 id = (AST.ident p)
                 newP = mkPattern (AST.name p) (AST.ident p) (AST.modifier p) (AST.ptype p)

-- --------------------------------------------------------------- [ Relations ]
doRelations :: RelationsExpr -> Patterns -> Relations
doRelations rs ps = reverse $ map (\x -> genRelation x ps) rs

genRelation :: RelationExpr -> Patterns -> Relation
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
