-- | The AST for the Sif Language.
module AST where

import Data.Maybe
import Data.List

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
-- | Type Heirarchy
data TyPattern = TyPattern
               | TyComponent
               | TySystem
               | TyDeployment
               | TyAdmin 
               | TyImplementation
               | TyImport -- ^ Nasty Hack 
               deriving (Show, Eq, Enum, Ord)

-- | Type Modifiers
data TyModifier = TyModAbstract
                | TyModConcrete
                deriving (Show, Eq, Read, Enum, Ord)

-- | Pattern Representation
data PatternExpr = PatternExpr {
      name       :: Maybe String,
      ident      :: ID,      
      origin     :: Maybe String,
      typ        :: TyPattern,
      modifier   :: TyModifier
    } deriving (Show, Eq)

-- Smart Constructors

-- | Make an import pattern
mkImportPattern :: ID -> String -> PatternExpr
mkImportPattern id origin = PatternExpr (Just id) id (Just origin) TyPattern TyModConcrete

-- | Make a pattern
mkPattern :: ID -> Maybe String -> TyPattern -> TyModifier -> PatternExpr
mkPattern id title typ mod = PatternExpr title id Nothing typ mod

-- --------------------------------------------------------------- [ Relations ]
-- | Type Heirarchy
data TyRelation = TyAssociation    -- ^ Association
                | TySpecialisation -- ^ Specialisation
                | TyRealisation    -- ^ Realisation
                | TyAggregation    -- ^ Aggregation
                deriving (Show, Eq, Enum, Ord)

-- | Relation Representation
data RelationExpr = RelationExpr {
      from  :: ID,
      to    :: ID,
      rtype :: TyRelation,
      desc  :: Maybe String
    } deriving (Show, Eq)

-- Smart Constructor for Relations

-- | Make a relation 
mkRelation :: ID -> ID -> TyRelation -> Maybe String -> RelationExpr
mkRelation from to typ desc = RelationExpr from to typ desc

-- -------------------------------------------------------- [ Accessor Methods ]
-- | Get Pattern
getPattern :: ID -> PatternsExpr -> Maybe PatternExpr
getPattern id [] = Nothing
getPattern id ps = find (\x -> ident x == id) ps

canNub :: Eq a => [a] -> Bool
canNub xs = if isNothing check
            then False
            else True
            where
              check = find (\x -> length x > 1) xs'
              xs' = map (\x -> elemIndices x xs) xs

  
-- --------------------------------------------------------------------- [ EOF ]
