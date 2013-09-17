-- | Smart constructors/Inference rules for Realisation relations.
module TypeSystem.RulesRealise where

import TypeSystem.Types

-- | Creates an realisation relation between an Implementation and Component pattern
--
-- E |- a : Implementation, E |- b : Component, E |- R'(a,b,R)
-- -----------------------------------------------------------
-- E |- a implements b : R
mkRealisesIC :: Implementation a -> Component a -> Maybe String -> Realisation a
mkRealisesIC (I i) (C c) desc = R $ Relation' i c desc

-- | Creates an realisation relation between an Implementation and Pattern pattern
--
-- E |- a : Implementation, E |- b : Pattern, E |- R'(a,b,R)
-- ---------------------------------------------------------
-- E |- a implements b : R
mkRealisesIP :: Implementation a -> Pattern a -> Maybe String -> Realisation a
mkRealisesIP (I i) (P p) desc = R $ Relation' i p desc

-- | Creates an realisation relation between an Pattern and Pattern pattern
--
-- E |- a : Pattern, E |- b : Pattern, E |- R'(a,b,R)
-- --------------------------------------------------
-- E |- a implements b : R
mkRealisesPP :: Pattern a -> Pattern a -> Maybe String -> Realisation a
mkRealisesPP (P p) (P q) desc = R $ Relation' p q desc

-- --------------------------------------------------------------------- [ EOF ]
