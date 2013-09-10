-- | The AST for the Sif Language.
module Model.AST where

import Data.Maybe
import Data.List
-- -------------------------------------------------------- [ Pattern Language ]
data PlangExpr = PlangExpr {
      title    :: String,
      label    :: ID,
      patterns :: PatternsExpr
    } deriving (Show)

-- ----------------------------------------------------------------- [ Pattern ]
data PatternExpr = PatternExpr {
      name       :: String,
      ident      :: ID,
      origin     :: Maybe String,
      modifier   :: Maybe ModifierExpr,
      extends    :: Maybe RelationsExpr,
      implements :: Maybe RelationsExpr,
      requires   :: Maybe RelationsExpr,
      links      :: Maybe RelationsExpr
    } deriving (Show, Eq)
    
-- --------------------------------------------------------------- [ Relations ]
data RelationExpr = RelationExpr {
      to   :: ID,
      desc :: Maybe String
    } deriving (Show, Eq)

data ModifierExpr = Abstract | Integration
                    deriving (Show, Eq, Read, Enum, Ord)

-- ------------------------------------------------------------ [ Type Aliases ]

type PatternsExpr  = [ PatternExpr ]
type RelationsExpr = [ RelationExpr ]
type IDs           = [ ID ]
type ID            = String

-- ------------------------------------------------------- [ Utility Functions ]

-- Pattern Creation

-- | Try to make a relation checking ID from list of existing patterns
-- Used during parsing
tryMkRelation :: ID -> PatternsExpr -> Maybe String -> Maybe RelationExpr
tryMkRelation id ps desc = res
                           where
                             p = getPattern id ps
                             res = case isNothing p of
                                     True -> Nothing
                                     otherwise -> Just $ RelationExpr (ident (fromJust p)) desc

-- | Make an import pattern
mkImportPattern :: ID -> String -> PatternExpr
mkImportPattern id origin = PatternExpr id id (Just origin) Nothing Nothing Nothing Nothing Nothing

-- | Make a simple pattern with no properties
mkSimplePattern :: String -> ID -> Maybe ModifierExpr -> PatternExpr
mkSimplePattern n id mod = PatternExpr n id Nothing mod Nothing Nothing Nothing Nothing

-- | Make a pattern with properties
mkComplexPattern :: String -> ID
                 -> Maybe ModifierExpr -- ^ Modifier
                 -> Maybe RelationsExpr -- ^ Extends
                 -> Maybe RelationsExpr -- ^ Realises
                 -> PatternExpr
mkComplexPattern n id mod exs imps = PatternExpr n id Nothing mod exs imps Nothing Nothing

-- -------------------------------------------------------- [ Accessor Methods ]
-- | Get Pattern
getPattern :: ID -> PatternsExpr -> Maybe PatternExpr
getPattern id [] = Nothing
getPattern id ps = find (\x -> ident x == id) ps

-- | In a list of patterns update the given pattern.
updatePatts :: PatternExpr -> PatternsExpr -> PatternsExpr
updatePatts p = map (\x -> if ident x == ident p then p else x)

-- Requires

-- | Add several requires relations to a pattern
addRequires :: RelationsExpr -> PatternExpr -> PatternExpr
addRequires rs p = case isNothing (requires p) of
                     True -> p {requires = Just rs}
                     otherwise -> p {requires = Just (rs ++ fromJust (requires p))}

-- | Add Require relation to Pattern
addRequire :: RelationExpr -> PatternExpr -> PatternExpr
addRequire r p = case isNothing (requires p) of
                   True -> p { requires = Just [r] }
                   otherwise -> p { requires = Just (r : fromJust (requires p))}

-- | Add several links relations to patterns
addLinks :: RelationsExpr -> PatternExpr -> PatternExpr
addLinks rs p = case isNothing (links p) of
                  True -> p {links = Just rs}
                  otherwise -> p {links = Just ( rs ++ fromJust (links p))}

-- | Add Link to Pattern
addLink :: RelationExpr -> PatternExpr -> PatternExpr
addLink r p = case isNothing (links p) of
                True -> p { links = Just [r] }
                otherwise -> p { links = Just (r : fromJust (links p))}




  
-- --------------------------------------------------------------------- [ EOF ]
