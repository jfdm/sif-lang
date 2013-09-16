-- | Smart constructors/Inference rules for Specialisation relations.
module TypeSystem.RulesSpecial where

import TypeSystem.Types

-- | Creates an specialisation relation between an Deployment and System pattern
--
-- E |- a : Deployment, E |- b : System, E |- E'(a,b,E)
-- ----------------------------------------------------
-- E |- a extends b : E
makeExtendsDS :: Deployment a -> System a -> Maybe String -> Specialisation a
makeExtendsDS (D d) (S s) desc = E $ Relation' d s desc

-- | Creates an specialisation relation between an Component and Component pattern
--
-- E |- a : Component, E |- b : Component, E |- E'(a,b,E)
-- ------------------------------------------------------
-- E |- a extends b : E
makeExtendsCC :: Component a -> Component a -> Maybe String -> Specialisation a
makeExtendsCC (C c) (C d) desc = E $ Relation' c d desc

-- | Creates an specialisation relation between an Component and Pattern pattern
--
-- E |- a : Component, E |- b : Pattern, E |- E'(a,b,E)
-- ----------------------------------------------------
-- E |- a extends b : E
makeExtendsCP :: Component a -> Pattern a -> Maybe String -> Specialisation a
makeExtendsCP (C c) (P p) desc = E $ Relation' c p desc

-- | Creates an specialisation relation between an Pattern and Pattern pattern
--
-- E |- a : Pattern, E |- b : Pattern, E |- E'(a,b,E)
-- --------------------------------------------------
-- E |- a extends b : E
makeExtendsPP :: Pattern a -> Pattern a -> Maybe String -> Specialisation a
makeExtendsPP (P p) (P q) desc = E $ Relation' p q desc

-- --------------------------------------------------------------------- [ EOF ]
