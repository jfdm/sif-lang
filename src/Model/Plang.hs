-- | Formal model of Patterns and Pattern Language
module Model.Plang
    ( Pattern,
             Deployment, System, Admin, Implementation

    ) where

-- ----------------------------------------------------- [ Pattern Definitions ]
data Pattern' = Pattern' {
      title :: String,
      ident :: String
    } deriving (Show)

newtype Pattern a = P Pattern'
newtype Deployment a = D Pattern'
newtype System a = S Pattern'
newtype Admin a = A Pattern'
newtype Component a = C Pattern'
newtype Implementation a = I Pattern'

-- ---------------------------------------------------- [ Pattern Constructors ]
mkPattern :: String -> String -> Pattern a
mkPattern t i = P $ Pattern' t i

mkDeployment :: String -> String -> Deployment a
mkDeployment t i = D $ Pattern' t i

mkSystem :: String -> String -> System a
mkSystem t i = S $ Pattern' t i

-- ---------------------------------------------------- [ Relation Definitions ]
data Relation' = Relation' {
      from :: String,
      to :: String,
      desc :: Maybe String
    } deriving (Show)

newtype Extends a = E Relation' -- ^ Specialisation
newtype Realises a = R Relation' -- ^ Realisation
newtype Utilises a = U Relation' -- ^ Aggregation
newtype Connected a = L Relation' -- ^ Association

-- -------------------------------------------------- [ Inference Rules Begin! ]

mkExtends :: Deployment a -> System a -> Maybe String -> Extends a
mkExtends (D d) (S s) desc = E $ mkRelation' d s desc

mkRealises :: Deployment a -> System a -> Maybe String -> Realises a
mkRealises (D d) (S s) desc = R $ mkRelation' d s desc

makUtilises :: System a -> Component a -> Maybe String -> Utilises a
makUtilises (S s) (C c) desc = U $ mkRelation' s c desc

mkRelation' :: Pattern' -> Pattern' -> Maybe String -> Relation'
mkRelation' t f d = Relation' (ident t) (ident f) d
-- modifier checks here?

-- --------------------------------------------------------------------- [ EOF ]
