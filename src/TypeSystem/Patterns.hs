-- | Smart constructors for our Sif Lang
module TypeSystem.Patterns where

import TypeSystem.Types


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

