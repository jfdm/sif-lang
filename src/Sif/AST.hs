-- | The AST for the Sif Language.
module Sif.AST where

import Data.Maybe
import Data.List

import Sif.Types

-- -------------------------------------------------------- [ Pattern Language ]
data PlangAST = PlangAST {
      title    :: String,
      label    :: ID,
      patterns :: PatternsExpr,
      relations :: RelationsExpr
    } deriving (Show)

-- ------------------------------------------------------------ [ Type Aliases ]
type PatternsExpr  = [ PatternExpr ]
type RelationsExpr = [ RelationExpr ]
type IDs           = [ ID ]
type ID            = String

-- ---------------------------------------------------------------- [ Patterns ]

-- | Pattern Representation
data PatternExpr = PatternExpr {
      name       :: Maybe String,
      ident      :: ID,      
      origin     :: Maybe String,
      ptype      :: TyGenPattern,
      modifier   :: TyModifier
    } deriving (Show, Eq)

-- Smart Constructors

-- | Make an import pattern
mkImportPatternExpr :: ID -> String -> PatternExpr
mkImportPatternExpr id origin = PatternExpr (Just id) id (Just origin) TyPattern TyModConcrete

-- | Make a pattern
mkPatternExpr :: ID -> Maybe String -> TyGenPattern -> TyModifier -> PatternExpr
mkPatternExpr id title = PatternExpr title id Nothing

-- --------------------------------------------------------------- [ Relations ]

-- | Relation Representation
data RelationExpr = RelationExpr {
      from  :: ID,
      to    :: ID,
      rtype :: TyRelation,
      desc  :: Maybe String
    } deriving (Show, Eq)

-- Smart Constructor for Relations

-- | Make a relation 
mkRelationExpr :: ID -> ID -> TyRelation -> Maybe String -> RelationExpr
mkRelationExpr = RelationExpr

-- -------------------------------------------------------- [ Accessor Methods ]
-- | Get Pattern
getPattern :: ID -> PatternsExpr -> Maybe PatternExpr
getPattern id [] = Nothing
getPattern id ps = find (\x -> ident x == id) ps

canNub :: Eq a => [a] -> Bool
canNub xs = isJust check
            where
              check = find (\x -> length x > 1) xs'
              xs' = map (`elemIndices` xs) xs

  
-- --------------------------------------------------------------------- [ EOF ]
