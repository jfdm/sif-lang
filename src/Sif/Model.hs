-- | Model for our Pattern Language
module Sif.Model where

import Data.Maybe
import Data.List

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

type Patterns = [Pattern]

-- | Definition of a relation between two patterns
data Relation = Relation {
      rtype :: TyRelation,   -- ^ The type of relation
      from  :: Pattern,      -- ^ Identifier of the originator
      to    :: Pattern,      -- ^ Identifier of the recipient
      desc  :: Maybe String  -- ^ A description if provided.
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
mkAssociates :: Pattern
             -> Pattern
             -> Maybe String
             -> Either String Relation
mkAssociates x y desc = Right $ Relation TyAssociation x y desc

-- | Define a specialisation relation between two patterns.
mkSpecial :: Pattern
          -> Pattern
          -> Maybe String
          -> Either String Relation
mkSpecial a b desc =
    case (ptype a, ptype b) of
      (TySystem, TySystem)       -> Right res
      (TyDeployment, TySystem)   -> Right res
      (TyComponent, TyComponent) -> Right res
      (TyComponent, TyPattern)   -> Right res
      (TyPattern, TyPattern)     -> Right res
      otherwise                  -> Left $ error "Invalid Specialisation"
    where
      res = Relation TySpecialisation a b desc

-- | Define a realisation relation between two patterns.      
mkRealise :: Pattern                 
          -> Pattern
          -> Maybe String
          -> Either String Relation
mkRealise a b desc =
    case (ptype a, ptype b) of 
      (TyComponent, TyComponent)      -> Right res
      (TyImplementation, TyComponent) -> Right res
      (TyImplementation, TyPattern)   -> Right res
      (TyPattern, TyPattern)          -> Right res
      otherwise                       -> Left $ error "Invalid Realisation"
    where
      res = Relation TyRealisation a b desc

-- | Define an aggregation relation between two patterns      
mkAggregate :: Pattern
            -> Pattern
            -> Maybe String
            -> Either String Relation
mkAggregate a b desc =
    case (ptype a, ptype b) of 
      (TyComponent, TyComponent)           -> Right res
      (TyComponent, TyPattern)             -> Right res
      (TySystem, TySystem)                 -> Right res
      (TySystem, TyDeployment)             -> Right res
      (TySystem, TyComponent)              -> Right res
      (TySystem, TyAdmin)                  -> Right res
      (TySystem, TyPattern)                -> Right res
      (TyImplementation, TyImplementation) -> Right res
      (TyPattern, TyPattern)               -> Right res
      otherwise                            -> Left $ error "Invalid Aggregation"
    where
      res = Relation TyAggregation a b desc

-- | Retrieve a pattern from an association list.
getPattern :: String -> Patterns -> Pattern
getPattern id ps = case isNothing res of
                     False -> fromJust res
                     otherwise -> error $ "Pattern with ID: " ++ id
                                        ++ " not found"
    where
      res = find (\x -> ident x == id) ps

-- --------------------------------------------------------------------- [ EOF ]



