-- | Types in our Sif Lang
module Sif.Types where

-- ----------------------------------------------------- [ Pattern Definitions ]

-- | Type Heirarchy
data TyGenPattern = TyPattern
                  | TyComponent
                  | TySystem
                  | TyDeployment
                  | TyAdmin 
                  | TyImplementation
                  | TyImport -- ^ Nasty Hack 
               deriving (Show, Eq, Enum, Ord)

-- | Type Modifiers
data TyModifier = TyModAbstract
                | TyModConcrete
                deriving (Show, Eq, Read, Enum, Ord)

-- | Type Heirarchy
data TyRelation = TyAssociation    -- ^ Association
                | TySpecialisation -- ^ Specialisation
                | TyRealisation    -- ^ Realisation
                | TyAggregation    -- ^ Aggregation
                deriving (Show, Eq, Enum, Ord)

-- --------------------------------------------------------------------- [ EOF ]
