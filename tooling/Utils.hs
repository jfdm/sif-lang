module Utils where

import Data.List
import Data.Maybe
import Model
import Examples

-- | Get Pattern
getPattern :: ID -> Patterns -> Maybe Pattern
getPattern id [] = Nothing
getPattern id ps = find (\x -> Model.ident x == id) ps


-- Update Pattern

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
                             

tryMkRelation :: ID -> Patterns -> Maybe String -> Maybe Relation
tryMkRelation id ps desc = do let p = getPattern id ps
                              case p of
                                Nothing -> Nothing
                                otherwise -> Just $ Relation (fromJust p) desc

