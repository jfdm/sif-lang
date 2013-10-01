-- | Model for our Pattern Language
module Sif.Model where

import Data.Maybe

import Sif.Types

-- | Definition of a pattern language.
data PlangSpec = PlangSpec {
      title     :: String,   -- ^ Title
      label     :: String,   -- ^ identifier
      imports   :: Patterns, -- ^ a possible list of imported patterns.
      patterns  :: Patterns, -- ^ a list of locally defined patterns.
      relations :: Relations -- ^ a list of relations on those patterns.
    } deriving (Show)

-- | During checking it can be useful to store a pattern as an association.
type PatternItem = (String, Pattern)

-- | Definition of a pattern.
data Pattern = Pattern {
      name      :: Maybe String, -- ^ Name of the pattern.
      ident     :: String,       -- ^ Its identifier
      origin    :: Maybe String, -- ^ The namespace origin. Nothing if local.
      modifier  :: TyModifier,   -- ^ Modifiers if specified. Default is Concrete.
      ptype     :: TyGenPattern  -- ^ Type of pattern.
    } deriving (Show)

type Patterns = [(String, Pattern)]

-- | Definition of a relation between two patterns
data Relation = Relation {
      rtype :: TyRelation,  -- ^ The type of relation
      from  :: String,      -- ^ Identifier of the originator
      to    :: String,      -- ^ Identifier of the recipient
      desc  :: Maybe String -- ^ A description if provided.
    } deriving (Show)

type Relations = [Relation]

-- ---------------------------------------------------- [ Pattern Constructors ]
-- | Construct a defined pattern in our model.
mkPattern :: Maybe String -- ^ Name
          -> String       -- ^ Identifier
          -> TyModifier   -- ^ Modifier
          -> TyGenPattern -- ^ pattern type
          -> Pattern
mkPattern n i = Pattern n i Nothing

-- | Construct an imported pattern in our model.
mkImpPattern :: Maybe String -- ^ name
             -> String       -- ^ Identifier
             -> Maybe String -- ^ Origin
             -> TyModifier   -- ^ Modifier
             -> TyGenPattern -- ^ Type
             -> Pattern
mkImpPattern = Pattern

-- | Create an association between two patterns.
mkAssociates :: PatternItem -> PatternItem -> Maybe String -> Relation
mkAssociates (x, _) (y, _) = Relation TyAssociation x y

-- | Define a specialisation relation between two patterns.
mkSpecial :: PatternItem -> PatternItem -> Maybe String -> Relation
mkSpecial (x, a) (y, b) desc =
    case (ptype a, ptype b) of
      (TyDeployment, TySystem)   -> res
      (TyComponent, TyComponent) -> res
      (TyComponent, TyPattern)   -> res
      (TyPattern, TyPattern)     -> res
      otherwise                  -> error "Invalid Specialisation"
    where
      res = Relation TySpecialisation x y desc

-- | Define a realisation relation between two patterns.      
mkRealise :: PatternItem -> PatternItem -> Maybe String -> Relation
mkRealise (x, a) (y, b) desc =
    case (ptype a, ptype b) of 
      (TyImplementation, TyComponent) -> res
      (TyImplementation, TyPattern)   -> res
      (TyPattern, TyPattern)          -> res
      otherwise                       -> error "Invalid Realisation"
    where
      res = Relation TyRealisation x y desc

-- | Define an aggregation relation between two patterns      
mkAggregate :: PatternItem -> PatternItem -> Maybe String -> Relation
mkAggregate (x, a) (y, b) desc =
    case (ptype a, ptype b) of 
      (TyComponent, TyComponent)           -> res
      (TyComponent, TyPattern)             -> res
      (TySystem, TySystem)                 -> res
      (TySystem, TyDeployment)             -> res
      (TySystem, TyComponent)              -> res
      (TySystem, TyAdmin)                  -> res
      (TySystem, TyPattern)                -> res
      (TyImplementation, TyImplementation) -> res
      otherwise                            -> error "Invalid Aggregation"
    where
      res = Relation TyAggregation x y desc

-- | Retrieve a pattern from an association list.
getPattern :: String -> Patterns -> PatternItem
getPattern id ps = case isNothing res of
                     False -> (id, fromJust res)
                     otherwise -> error "oops"
    where
      res = lookup id ps

-- --------------------------------------------------------------------- [ EOF ]
