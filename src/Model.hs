-- | Model for our Pattern Language
module Model where

import Data.Maybe
import Types

data PlangSpec = PlangSpec {
      title     :: String,
      label     :: String,
      imports   :: Patterns,      
      patterns  :: Patterns,
      relations :: Relations
    } deriving (Show)

type Patterns = [(String, Pattern)]
type PatternItem = (String, Pattern)

data Pattern = Pattern {
      name      :: Maybe String,
      ident     :: String,
      origin    :: Maybe String,
      modifier  :: TyModifier,
      ptype     :: TyGenPattern
    } deriving (Show)

type Relations = [Relation]
data Relation = Relation {
      rtype :: TyRelation,
      from :: String,
      to :: String,
      desc :: Maybe String
    } deriving (Show)

mkPattern :: Maybe String -> String -> TyModifier -> TyGenPattern -> Pattern
mkPattern n i m t = Pattern n i Nothing m t

mkImpPattern :: Maybe String -> String -> Maybe String -> TyModifier -> TyGenPattern -> Pattern
mkImpPattern n i o m t = Pattern n i o m t

mkAssociates :: PatternItem -> PatternItem -> Maybe String -> Relation
mkAssociates x y desc = Relation TyAssociation x' y' desc
                        where
                          x' = fst x
                          y' = fst y

mkSpecial :: PatternItem -> PatternItem -> Maybe String -> Relation
mkSpecial x y desc = case (a,b) of
                       (TyDeployment, TySystem) -> res
                       (TyComponent, TyComponent) -> res
                       (TyComponent, TyPattern) -> res
                       (TyPattern, TyPattern) -> res
                       otherwise -> error "Invalid Specialisation"
                     where
                       res = Relation TySpecialisation x' y' desc
                       (a,b) = (ptype (snd x), ptype (snd y))
                       x' = fst x
                       y' = fst y

mkRealise :: PatternItem -> PatternItem -> Maybe String -> Relation
mkRealise x y desc = case (a,b) of 
                       (TyImplementation, TyComponent) -> res
                       (TyImplementation, TyPattern) -> res
                       (TyPattern, TyPattern) -> res
                       otherwise -> error "Invalid Realisation"
                     where
                       res = Relation TyRealisation x' y' desc
                       (a,b) = (ptype (snd x), ptype (snd y))
                       x' = fst x
                       y' = fst y

mkAggregate :: PatternItem -> PatternItem -> Maybe String -> Relation
mkAggregate x y desc = case (a,b) of 
                         (TyComponent, TyComponent) -> res
                         (TyComponent, TyPattern) -> res
                         (TySystem, TySystem) -> res
                         (TySystem, TyDeployment) -> res
                         (TySystem, TyComponent) -> res
                         (TySystem, TyAdmin) -> res
                         (TySystem, TyPattern) -> res
                         (TyImplementation, TyImplementation) -> res
                         otherwise -> error "Invalid Aggregation"
                       where
                         res = Relation TyAggregation x' y' desc
                         (a,b) = (ptype (snd x), ptype (snd y))
                         x' = fst x
                         y' = fst y                      

getPattern :: String -> Patterns -> PatternItem
getPattern id ps = case isNothing res of
                     False -> (id, fromJust res)
                     otherwise -> error "oops"
    where
      res = lookup id ps
-- --------------------------------------------------------------------- [ EOF ]
