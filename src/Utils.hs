-- | Utility functions.
module Utils where

import Data.List
import Data.Bool
import Data.Maybe
import Model

-- ------------------------------------------------------- [ Pattern Functions ]

-- | Is Pattern A linked to Pattern B
isLinkedTo :: Pattern -> Pattern -> Bool
isLinkedTo a b = relationCheck' (Model.links b) a

-- | Is Pattern A an extension of Pattern B
anExtensionOf :: Pattern -> Pattern -> Bool
anExtensionOf a b = relationCheck' (Model.extends b) a

-- | Is Pattern A an implementation of Pattern B
isImplementionOf :: Pattern -> Pattern -> Bool
isImplementionOf a b = relationCheck' (Model.implements b) a

-- | Is Pattern A used by pattern B
isUsedBy :: Pattern -> Pattern -> Bool
isUsedBy a b = relationCheck' (Model.requires b) a

-- | Does Pattern A has a relation to Pattern B
hasRelation :: Pattern -> Pattern -> Bool
hasRelation a b = isLinkedTo a b
                  || anExtensionOf a b
                  || isImplementionOf a b
                  || isUsedBy a b

-- | DO the relation check
relationCheck' :: Maybe Relations -> Pattern -> Bool
relationCheck' Nothing p = False
relationCheck' (Just rs) p = isJust $ find (\r -> Model.ident p == Model.to r) rs

-- | Does the pattern have properties.
hasProperties :: Pattern -> Bool
hasProperties p = isJust (Model.extends p) || isJust (Model.implements p)

-- | In a list of patterns update the given pattern.
updatePatts :: Pattern -> Patterns -> Patterns
updatePatts p = map (\x -> if Model.ident x == Model.ident p then p else x)
  
-- ------------------------------------------------------------------- [ Links ]

-- | Add several links relations to patterns
addLinks :: Relations -> Pattern -> Pattern
addLinks rs p = case isNothing (Model.links p) of
                  True -> p {links = Just rs}
                  otherwise -> p {links = Just ( rs ++ fromJust (Model.links p))}

-- | Add Link to Pattern
addLink :: Relation -> Pattern -> Pattern
addLink r p = case isNothing (Model.links p) of
                True -> p { links = Just [r] }
                otherwise -> p { links = Just (r : fromJust (Model.links p))}


-- ---------------------------------------------------------------- [ Requires ]                
-- | Add several requires relations to a pattern
addRequires :: Relations -> Pattern -> Pattern
addRequires rs p = case isNothing (Model.requires p) of
                     True -> p {requires = Just rs}
                     otherwise -> p {requires = Just (rs ++ fromJust (Model.requires p))}

-- | Add Require relation to Pattern
addRequire :: Relation -> Pattern -> Pattern
addRequire r p = case isNothing (Model.requires p) of
                   True -> p { requires = Just [r] }
                   otherwise -> p { requires = Just (r : fromJust (Model.requires p))}

-- ------------------------------------------------------------- [ Get Pattern ]
-- | Get Pattern
getPattern :: ID -> Patterns -> Maybe Pattern
getPattern id [] = Nothing
getPattern id ps = find (\x -> Model.ident x == id) ps

-- ------------------------------------------------------- [ Creation patterns ]

-- | Try to make a relation checking ID from list of existing patterns
-- Used during parsing
tryMkRelation :: ID -> Patterns -> Maybe String -> Maybe Relation
tryMkRelation id ps desc = res
                           where
                             p = getPattern id ps
                             res = case isNothing p of
                                     True -> Nothing
                                     otherwise -> Just $ Relation (Model.ident (fromJust p)) desc

-- | Make an import pattern
mkImportPattern :: ID -> String -> Pattern
mkImportPattern id origin = Pattern id id (Just origin) Nothing Nothing Nothing Nothing Nothing

-- | Make a simple pattern with no properties
mkSimplePattern :: String -> ID -> Maybe Modifier -> Pattern
mkSimplePattern n id mod = Pattern n id Nothing mod Nothing Nothing Nothing Nothing

-- | Make a pattern with properties
mkComplexPattern :: String -> ID -> Maybe Modifier -> Maybe Extends -> Maybe Realises -> Pattern
mkComplexPattern n id mod exs imps = Pattern n id Nothing mod exs imps Nothing Nothing

-- | Get the origin on the pattern.
getImportOrigin :: Pattern -> String
getImportOrigin p  = fromJust $ Model.origin p
