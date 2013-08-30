module Utils where

import Data.List
import Data.Maybe
import Model

-- | Get Pattern
getPattern :: ID -> Patterns -> Maybe Pattern
getPattern id [] = Nothing
getPattern id ps = find (\x -> Model.ident x == id) ps

-- | Add Link to Pattern
addLink :: ID -> Relation -> Patterns -> Patterns
addLink id l ps = map (\p -> if Model.ident p == id then update p l else p) ps
    where
      update p l = case isNothing (Model.links p) of
                     True -> p { links = Just [l] }
                     otherwise -> p { links = Just (l : fromJust (Model.links p))}

-- | Add Require to Pattern
addRequire :: ID -> Relation -> Patterns -> Patterns
addRequire id l ps = map (\p -> if Model.ident p == id then update p l else p) ps
    where
      update p l = case isNothing (Model.requires p) of
                     True -> p { requires = Just [l]}
                     otherwise -> p { requires = Just (l : fromJust (Model.requires p))}
                             
-- | Try to make a relation checking ID from list of existing patterns
-- Used during parsing
tryMkRelation :: ID -> Patterns -> Maybe String -> Maybe Relation
tryMkRelation id ps desc = res
                           where
                             p = getPattern id ps
                             res = case isNothing p of
                                     True -> Nothing
                                     otherwise -> Just $ Relation (Model.ident (fromJust p)) desc


mkImportPattern :: ID -> String -> Pattern
mkImportPattern id origin = Pattern id id (Just origin) Nothing Nothing Nothing Nothing Nothing

mkSimplePattern :: String -> ID -> Maybe Modifier -> Pattern
mkSimplePattern n id mod = Pattern n id Nothing mod Nothing Nothing Nothing Nothing

mkComplexPattern :: String -> ID -> Maybe Modifier -> Maybe Extends -> Maybe Realises -> Pattern
mkComplexPattern n id mod exs imps = Pattern n id Nothing mod exs imps Nothing Nothing
