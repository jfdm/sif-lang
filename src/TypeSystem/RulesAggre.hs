-- | Smart constructors/Inference rules for Aggregation relations.
module TypeSystem.RulesAggre where

import TypeSystem.Types

-- | Creates an aggregation relation between an Component and Component pattern
--
-- E |- a : Component, E |- b : Component, E |- U'(a,b,U)
-- ------------------------------------------------------
-- E |- a uses b : U
mkAggreCC :: Component a -> Component a -> Maybe String -> Aggregation a
mkAggreCC (C c) (C d) desc = U $ Relation' c d desc

-- | Creates an aggregation relation between an Component and Pattern pattern
--
-- E |- a : Component, E |- b : Pattern, E |- U'(a,b,U)
-- ----------------------------------------------------
-- E |- a uses b : U
mkAggreCP :: Component a -> Pattern a -> Maybe String -> Aggregation a
mkAggreCP (C c) (P p) desc = U $ Relation' c p desc

-- | Creates an aggregation relation between an System and System pattern
--
-- E |- a : System, E |- b : System, E |- U'(a,b,U)
-- ------------------------------------------------
-- E |- a uses b : U
mkAggreSS :: System a -> System a -> Maybe String -> Aggregation a
mkAggreSS (S s) (S t) desc = U $ Relation' s t desc

-- | Creates an aggregation relation between an System and Deployment pattern
--
-- E |- a : System, E |- b : Deployment, E |- U'(a,b,U)
-- ----------------------------------------------------
-- E |- a uses b : U
mkAggreSD :: System a -> Deployment a -> Maybe String -> Aggregation a
mkAggreSD (S s) (D d) desc = U $ Relation' s d desc

-- | Creates an aggregation relation between an System and Component pattern
--
-- E |- a : System, E |- b : Component, E |- U'(a,b,U)
-- ---------------------------------------------------
-- E |- a uses b : U
mkAggreSC :: System a -> Component a -> Maybe String -> Aggregation a
mkAggreSC (S s) (C c) desc = U $ Relation' s c desc

-- | Creates an aggregation relation between an System and Admin pattern
--
-- E |- a : System, E |- b : Admin, E |- U'(a,b,U)
-- -----------------------------------------------
-- E |- a uses b : U
mkAggreSA :: System a -> Admin a -> Maybe String -> Aggregation a
mkAggreSA (S s) (A a) desc = U $ Relation' s a desc

-- | Creates an aggregation relation between an System and Pattern pattern
--
-- E |- a : System, E |- b : Pattern, E |- U'(a,b,U)
-- -------------------------------------------------
-- E |- a uses b : U
mkAggreSP :: System a -> Pattern a -> Maybe String -> Aggregation a
mkAggreSP (S s) (P p) desc = U $ Relation' s p desc

-- | Creates an aggregation relation between an Implementation and Implementation pattern
--
-- E |- a : Implementation, E |- b : Implementation, E |- U'(a,b,U)
-- ----------------------------------------------------------------
-- E |- a uses b : U
mkAggreII :: Implementation a -> Implementation a -> Maybe String -> Aggregation a
mkAggreII (I i) (I j) desc = U $ Relation' i j desc

-- --------------------------------------------------------------------- [ EOF ]
