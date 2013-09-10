-- | Utility functions.
module Utils where

import Data.List
import Data.Bool
import Data.Maybe
import Model.AST

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

-- | Get the origin on the pattern.
getImportOrigin :: Pattern -> String
getImportOrigin p  = fromJust $ Model.origin p
