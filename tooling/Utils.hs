module Utils where

import Data.List
import Data.Maybe
import Model
import Examples

-- | Get Pattern
getPattern :: ID -> Patterns -> Maybe Pattern
getPattern id [] = Nothing
getPattern id ps = find (\x -> Model.identity x == id) ps


-- Update Pattern

-- | Add Link to Pattern
addLink :: ID -> Relation -> Patterns -> Patterns
addLink id l [] = []
addLink id l ps = map (\p -> if Model.identity p == id then update p l else p) ps
                  where
                    update p l = case isNothing (Model.links p) of
                                   True -> p
                                   otherwise -> p { links = Just (l : fromJust (Model.links p))}

-- | Add Require to Pattern
addRequire :: ID -> Relation -> Patterns -> Patterns
addRequire id l [] = []
addRequire id l ps = map (\p -> if Model.identity p == id then update p l else p) ps
                  where
                    update p l = case isNothing (Model.requires p) of
                                   True -> p
                                   otherwise -> p { links = Just (l : fromJust (Model.requires p))}
