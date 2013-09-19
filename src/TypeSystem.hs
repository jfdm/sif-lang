-- | Model for our Pattern Language
module TypeSystem where

import Data.Maybe
import qualified Data.Map as Map  
import AST
import Types

type PMap = Map.Map String Pattern 

data PlangSpec = PlangSpec {
      label    :: String,
      patterns :: PMap,
      relations :: Relations
    } deriving (Show)

type PatternValues = [(String, Pattern)]
data Pattern = Pattern {
      name :: Maybe String,
      mod :: TyModifier,
      ptype :: TyGenPattern
    } deriving (Show)

type Relations = [Relation]
data Relation = Relation {
      rtype :: TyRelation,
      from :: Pattern,
      to :: Pattern,
      desc :: Maybe String
    } deriving (Show)

mkAssociates :: Pattern -> Pattern -> Maybe String -> Relation
mkAssociates x y desc = Relation TyAssociation x y desc

mkSpecial :: Pattern -> Pattern -> Maybe String -> Relation
mkSpecial x y desc = case (a,b) of
                       (TyDeployment, TySystem) -> res
                       (TyComponent, TyComponent) -> res
                       (TyComponent, TyPattern) -> res
                       (TyPattern, TyPattern) -> res
                       otherwise -> error "Invalid Specialisation"
                     where
                       res = Relation TySpecialisation x y desc
                       (a,b) = (ptype x, ptype y)

mkRealise :: Pattern -> Pattern -> Maybe String -> Relation
mkRealise x y desc = case (a,b) of 
                       (TyImplementation, TyComponent) -> res
                       (TyImplementation, TyPattern) -> res
                       (TyPattern, TyPattern) -> res
                       otherwise -> error "Invalid Realisation"
                     where
                       res = Relation TyRealisation x y desc
                       (a,b) = (ptype x, ptype y)

mkAggregate :: Pattern -> Pattern -> Maybe String -> Relation
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
                         res = Relation TyAggregation x y desc
                         (a,b) = (ptype x, ptype y)

getPattern :: String -> PMap -> Pattern
getPattern id ps = case isNothing res of
                     False -> fromJust res
                     otherwise -> error "oops"
    where
      res = Map.lookup id ps
-- --------------------------------------------------------------------- [ EOF ]
