-- | Functions to transform the Plang AST into the formal
-- representation.
-- 
module Sif.Checker (chkPlangSpec) where

import Data.Maybe
import Data.List

import Sif.AST as AST hiding (getPattern)
import Sif.Model
import Sif.Types
import Sif.Parser

import Debug.Trace
import Text.Show.Pretty

-- | Do the transformation
chkPlangSpec :: PlangAST -> Either String PlangSpec
chkPlangSpec spec = case rs of
                      Left err  -> Left err
                      Right rs' -> Right $ PlangSpec tit lab is ps rs'
    where
      tit = AST.title spec
      lab = AST.label spec
      ps = doPatterns ps'
      is = doImports is'
      (is', ps') =  break (isNothing . AST.origin) (AST.patterns spec)
      rs = doRelations (AST.relations spec) (ps ++ is)

-- ----------------------------------------------------------------- [ Imports ]
-- | Identifiy the differnent imported namespaces and convert each
-- group.
doImports :: PatternsExpr -> Patterns
doImports [] = []
doImports is = concatMap getImportGroups groups
    where
      groups = groupBy groupImports is
      groupImports x y = AST.origin x == AST.origin y

-- | Convert each pattern in the group of imports
getImportGroups :: PatternsExpr -> Patterns
getImportGroups [] = []
getImportGroups is = map genImpPattern is


genImpPattern :: PatternExpr -> Pattern
genImpPattern i = mkImpPattern (AST.name     i)
                               (AST.ident    i)
                               (AST.origin   i)
                               (AST.modifier i)
                               (AST.ptype    i)
-- Plan for dealing with imports
-- 1. Locate plang specification
-- 2. Read in spec
-- 3. Extract imported Pattern.

-- ---------------------------------------------------------------- [ Patterns ]
-- | Deal with patterns
doPatterns :: PatternsExpr -> Patterns
doPatterns ps = map genPattern ps

-- | Deal with a pattern
genPattern :: PatternExpr -> Pattern
genPattern p = mkPattern (AST.name p) (AST.ident p) (AST.modifier p) (AST.ptype p)

-- --------------------------------------------------------------- [ Relations ]
-- | Deal with relations
-- remember to reverse
doRelations :: RelationsExpr -> Patterns -> Either String Relations
doRelations rs ps = sequence (map (`genRelation` ps) (reverse rs))

-- | Deal with a relation
genRelation :: RelationExpr -> Patterns -> Either String Relation
genRelation r ps = case AST.rtype r of
                     TyAssociation    -> mkAssociates x y desc
                     TySpecialisation -> mkSpecial    x y desc
                     TyAggregation    -> mkAggregate  x y desc
                     TyRealisation    -> mkRealise    x y desc
                   where
                     desc = AST.desc r
                     x = getPattern (AST.from r) ps
                     y = getPattern (AST.to r) ps

-- --------------------------------------------------------------------- [ EOF ]
