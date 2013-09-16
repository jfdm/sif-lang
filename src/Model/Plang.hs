-- | Formal model of Patterns and Pattern Language
module Model.Plang
    ( Pattern, Deployment, System, Admin, Implementation,
      Modifier(..)

    ) where

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
newtype Pattern a = P Pattern'
newtype Deployment a = D Pattern'
newtype System a = S Pattern'
newtype Admin a = A Pattern'
newtype Component a = C Pattern'
newtype Implementation a = I Pattern'

-- ---------------------------------------------------- [ Pattern Constructors ]

-- | Pattern 
--
-- ----------------
-- E |- p : Pattern
mkPattern :: String -> String -> Maybe Modifier -> Pattern a
mkPattern t i m = P $ Pattern' t i m

-- | Deployment Pattern
--
-- -------------------
-- E |- p : Deployment
mkDeployment :: String -> String -> Maybe Modifier -> Deployment a
mkDeployment t i m = D $ Pattern' t i m

-- | System Pattern
--
-- ---------------
-- E |- p : System
mkSystem :: String -> String -> Maybe Modifier -> System a
mkSystem t i m = S $ Pattern' t i m

-- | Administration Pattern
--
-- --------------
-- E |- p : Admin
mkAdmin :: String -> String -> Maybe Modifier -> Admin a
mkAdmin t i m = A $ Pattern' t i m

-- | Component Pattern
--
-- ------------------
-- E |- p : Component
mkComponent :: String -> String -> Maybe Modifier -> Component a
mkComponent t i m = C $ Pattern' t i m

-- | Implementation Patterns
-- 
-- -----------------------
-- E |- p : Implementation
mkImplementation :: String -> String -> Maybe Modifier -> Implementation a
mkImplementation t i m = I $ Pattern' t i m

-- ---------------------------------------------------- [ Relation Definitions ]
data Relation' = Relation' {
      from :: String,
      to :: String,
      desc :: Maybe String
    } deriving (Show)

-- | Relation Heirarchy
-- TyRelation ::= Association | Specialisation | Realisation | Aggregation ;
newtype Specialisation a = E Relation'
newtype Realisation a = R Relation'
newtype Aggregation a = U Relation'
newtype Association a = L Relation'

-- -------------------------------------------------- [ Inference Rules Begin! ]
-- Specialisation
{-
Deployment a -> System a

Component a -> Component a
Component a -> Pattern a
Pattern a -> Pattern a

-}
-- Realisation
{-

Implementation a -> Component a
Implementation a -> Pattern a

Pattern a -> Pattern a
-}

-- ------------------------------------------------------------- [ Aggregation ]

{-
Component a -> Component a-> Maybe String -> Aggregation a
Component a -> Pattern a -> Maybe String -> Aggregation a

System a -> System a -> Maybe String -> Aggregation a
System a -> Deployment a -> Maybe String -> Aggregation a
System a -> Component a -> Maybe String -> Aggregation a
System a -> Admin a -> Maybe String -> Aggregation a
System a -> Pattern a -> Maybe String -> Aggregation a

Implementation a -> Implementation a -> Maybe String -> Aggregation a
-}

-- | Creates an agggregation relation between an implementaion pattern and pattern.
mkUsesIP :: Implementation a -> Pattern a -> Maybe String -> Aggregation a
mkUsesIP (I i) (P p) desc = U $ mkRelation' i p desc

-- | Creates an aggregation relation between two Patterns
--
-- E |- a : Pattern,  E |- b : Pattern, E |- U'(a,b,U)
-- ---------------------------------------------------
-- E |- a uses b : U
mkUsesPP :: Pattern a -> Pattern a -> Maybe String -> Aggregation a
mkUsesPP (P s) (P p) desc = U $ mkRelation' s p desc

-- ------------------------------------------------------------- [ Association ]
-- | Pattern is *linked to* Pattern
-- Nasty Hack
mkAssoc :: String -> String -> Maybe String -> Association a
mkAssoc from to desc = L $ Relation' from to desc

-- | System         | 'o-' | Component | System Pattern *uses* Component Pattern                |

-- |----------------+------+-----------+--------------------------------------------------------|
-- | Pattern        | '->' | Pattern   | Pattern is *linked* pattern                            |
-- |----------------+------+-----------+--------------------------------------------------------|
-- | Concrete       | '=>' | Abstract  | Concrete pattern *implements* Abstract pattern.        |
-- | Composite      | 'o-' | Primitive | Composite pattern *uses* Primitive/Comp Pattern        |

-- Deployment Pattern *extends* System Pattern
mkExtends :: Deployment a -> System a -> Maybe String -> Specialisation a
mkExtends (D d) (S s) desc = E $ mkRelation' d s desc

-- Implementation Pattern *implements* Component Patterns
mkRealises :: Implementation a -> Component a -> Maybe String -> Realisation a
mkRealises (I i) (C s) desc = R $ mkRelation' i s desc

-- | 


 -- Generic Relation Constructor.

mkRelation' :: Pattern' -> Pattern' -> Maybe String -> Relation'
mkRelation' t f d = Relation' (ident t) (ident f) d
-- modifier checks here?

-- --------------------------------------------------------------------- [ EOF ]
