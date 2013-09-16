-- | Smart constructors/Inference rules for Realisation relations.
module TypeSystem.RulesRealise where

import TypeSystem.Types

-- | Creates an realisation relation between an Implementation and Component pattern
--
-- E |- a : Implementation, E |- b : Component, E |- R'(a,b,R)
-- -----------------------------------------------------------
-- E |- a implements b : R
makeRealisesIC :: Implementation a -> Component a -> Maybe String -> Realisation a
makeRealisesIC (I i) (C c) desc = R $ Relation' i c desc

-- | Creates an realisation relation between an Implementation and Pattern pattern
--
-- E |- a : Implementation, E |- b : Pattern, E |- R'(a,b,R)
-- ---------------------------------------------------------
-- E |- a implements b : R
makeRealisesIP :: Implementation a -> Pattern a -> Maybe String -> Realisation a
makeRealisesIP (I i) (P p) desc = R $ Relation' i p desc

-- | Creates an realisation relation between an Pattern and Pattern pattern
--
-- E |- a : Pattern, E |- b : Pattern, E |- R'(a,b,R)
-- --------------------------------------------------
-- E |- a implements b : R
makeRealisesPP :: Pattern a -> Pattern a -> Maybe String -> Realisation a
makeRealisesPP (P p) (P q) desc = R $ Relation' p q desc
