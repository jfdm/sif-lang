-- | Smart constructors/Inference rules for Aggregation relations.
module TypeSystem.RulesAggre where

import TypeSystem.Types

-- | Creates an aggregation relation between an Component and Component pattern
--
-- E |- a : Component, E |- b : Component, E |- U'(a,b,U)
-- ------------------------------------------------------
-- E |- a uses b : U
makeAggreCC :: Component a -> Component a -> Maybe String -> Aggregation a
makeAggreCC (C c) (C d) desc = U $ Relation' c d desc

-- | Creates an aggregation relation between an Component and Pattern pattern
--
-- E |- a : Component, E |- b : Pattern, E |- U'(a,b,U)
-- ----------------------------------------------------
-- E |- a uses b : U
makeAggreCP :: Component a -> Pattern a -> Maybe String -> Aggregation a
makeAggreCP (C c) (P p) desc = U $ Relation' c p desc

-- | Creates an aggregation relation between an System and System pattern
--
-- E |- a : System, E |- b : System, E |- U'(a,b,U)
-- ------------------------------------------------
-- E |- a uses b : U
makeAggreSS :: System a -> System a -> Maybe String -> Aggregation a
makeAggreSS (S s) (S t) desc = U $ Relation' s t desc

-- | Creates an aggregation relation between an System and Deployment pattern
--
-- E |- a : System, E |- b : Deployment, E |- U'(a,b,U)
-- ----------------------------------------------------
-- E |- a uses b : U
makeAggreSD :: System a -> Deployment a -> Maybe String -> Aggregation a
makeAggreSD (S s) (D d) desc = U $ Relation' s d desc

-- | Creates an aggregation relation between an System and Component pattern
--
-- E |- a : System, E |- b : Component, E |- U'(a,b,U)
-- ---------------------------------------------------
-- E |- a uses b : U
makeAggreSC :: System a -> Component a -> Maybe String -> Aggregation a
makeAggreSC (S s) (C c) desc = U $ Relation' s c desc

-- | Creates an aggregation relation between an System and Admin pattern
--
-- E |- a : System, E |- b : Admin, E |- U'(a,b,U)
-- -----------------------------------------------
-- E |- a uses b : U
makeAggreSA :: System a -> Admin a -> Maybe String -> Aggregation a
makeAggreSA (S s) (A a) desc = U $ Relation' s a desc

-- | Creates an aggregation relation between an System and Pattern pattern
--
-- E |- a : System, E |- b : Pattern, E |- U'(a,b,U)
-- -------------------------------------------------
-- E |- a uses b : U
makeAggreSP :: System a -> Pattern a -> Maybe String -> Aggregation a
makeAggreSP (S s) (P p) desc = U $ Relation' s p desc

-- | Creates an aggregation relation between an Implementation and Implementation pattern
--
-- E |- a : Implementation, E |- b : Implementation, E |- U'(a,b,U)
-- ----------------------------------------------------------------
-- E |- a uses b : U
makeAggreII :: Implementation a -> Implementation a -> Maybe String -> Aggregation a
makeAggreII (I i) (I j) desc = U $ Relation' i j desc

-- --------------------------------------------------------------------- [ EOF ]
