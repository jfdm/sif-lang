-- | Types in our Sif Lang
module TypeSystem.Types where

-- ----------------------------------------------------- [ Pattern Definitions ]
-- | Pattern Body
data Pattern' = Pattern' {
      title :: String,
      ident :: String,
      mod   :: Maybe Modifier
    } deriving (Show)

-- | Type Modifiers
data Modifier = Abstract
              | Concrete
                deriving (Show, Eq, Read, Enum, Ord)

-- | Type Heirarchy
-- TyPattern ::= Pattern | Deployment | System | Admin | Component | Implementation
newtype Pattern a        = P Pattern'
newtype Deployment a     = D Pattern'
newtype System a         = S Pattern'
newtype Admin a          = A Pattern'
newtype Component a      = C Pattern'
newtype Implementation a = I Pattern'


-- ---------------------------------------------------- [ Relation Definitions ]
data Relation' = Relation' {
      from :: Pattern',
      to :: Pattern',
      desc :: Maybe String
    } deriving (Show)

-- | Relation Heirarchy
-- TyRelation ::= Association | Specialisation | Realisation | Aggregation ;
newtype Specialisation a = E Relation'
newtype Realisation a = R Relation'
newtype Aggregation a = U Relation'
newtype Association a = L Relation'

-- --------------------------------------------------------------------- [ EOF ]
