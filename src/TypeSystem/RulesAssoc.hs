-- | Smart Constructors/Inference Rules for Association between patterns.
module TypeSystem.RulesAssoc where

import TypeSystem.Types

-- | Creates an association relation between an Pattern and Deployment pattern
--
-- E |- a : Pattern, E |- b : Deployment, E |- L'(a,b,L)
-- -----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPD :: Pattern a -> Deployment a -> Maybe String -> Association a
makeAssocPD (P p) (D d) desc = L $ Relation' p d desc

-- | Creates an association relation between an Pattern and System pattern
--
-- E |- a : Pattern, E |- b : System, E |- L'(a,b,L)
-- -------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPS :: Pattern a -> System a -> Maybe String -> Association a
makeAssocPS (P p) (S s) desc = L $ Relation' p s desc

-- | Creates an association relation between an Pattern and Admin pattern
--
-- E |- a : Pattern, E |- b : Admin, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPA :: Pattern a -> Admin a -> Maybe String -> Association a
makeAssocPA (P p) (A a) desc = L $ Relation' p a desc

-- | Creates an association relation between an Pattern and Component pattern
--
-- E |- a : Pattern, E |- b : Component, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPC :: Pattern a -> Component a -> Maybe String -> Association a
makeAssocPC (P p) (C c) desc = L $ Relation' p c desc

-- | Creates an association relation between an Pattern and Implementation pattern
--
-- E |- a : Pattern, E |- b : Implementation, E |- L'(a,b,L)
-- ---------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPI :: Pattern a -> Implementation a -> Maybe String -> Association a
makeAssocPI (P p) (I i) desc = L $ Relation' p i desc

-- | Creates an association relation between an Deployment and Pattern pattern
--
-- E |- a : Deployment, E |- b : Pattern, E |- L'(a,b,L)
-- -----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDP :: Deployment a -> Pattern a -> Maybe String -> Association a
makeAssocDP (D d) (P p) desc = L $ Relation' d p desc

-- | Creates an association relation between an Deployment and System pattern
--
-- E |- a : Deployment, E |- b : System, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDS :: Deployment a -> System a -> Maybe String -> Association a
makeAssocDS (D d) (S s) desc = L $ Relation' d s desc

-- | Creates an association relation between an Deployment and Admin pattern
--
-- E |- a : Deployment, E |- b : Admin, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDA :: Deployment a -> Admin a -> Maybe String -> Association a
makeAssocDA (D d) (A a) desc = L $ Relation' d a desc

-- | Creates an association relation between an Deployment and Component pattern
--
-- E |- a : Deployment, E |- b : Component, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDC :: Deployment a -> Component a -> Maybe String -> Association a
makeAssocDC (D d) (C c) desc = L $ Relation' d c desc

-- | Creates an association relation between an Deployment and Implementation pattern
--
-- E |- a : Deployment, E |- b : Implementation, E |- L'(a,b,L)
-- ------------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDI :: Deployment a -> Implementation a -> Maybe String -> Association a
makeAssocDI (D d) (I i) desc = L $ Relation' d i desc

-- | Creates an association relation between an System and Pattern pattern
--
-- E |- a : System, E |- b : Pattern, E |- L'(a,b,L)
-- -------------------------------------------------
-- E |- a linkedTo b : L
makeAssocSP :: System a -> Pattern a -> Maybe String -> Association a
makeAssocSP (S s) (P p) desc = L $ Relation' s p desc

-- | Creates an association relation between an System and Deployment pattern
--
-- E |- a : System, E |- b : Deployment, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocSD :: System a -> Deployment a -> Maybe String -> Association a
makeAssocSD (S s) (D d) desc = L $ Relation' s d desc

-- | Creates an association relation between an System and Admin pattern
--
-- E |- a : System, E |- b : Admin, E |- L'(a,b,L)
-- -----------------------------------------------
-- E |- a linkedTo b : L
makeAssocSA :: System a -> Admin a -> Maybe String -> Association a
makeAssocSA (S s) (A a) desc = L $ Relation' s a desc

-- | Creates an association relation between an System and Component pattern
--
-- E |- a : System, E |- b : Component, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
makeAssocSC :: System a -> Component a -> Maybe String -> Association a
makeAssocSC (S s) (C c) desc = L $ Relation' s c desc

-- | Creates an association relation between an System and Implementation pattern
--
-- E |- a : System, E |- b : Implementation, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocSI :: System a -> Implementation a -> Maybe String -> Association a
makeAssocSI (S s) (I i) desc = L $ Relation' s i desc

-- | Creates an association relation between an Admin and Pattern pattern
--
-- E |- a : Admin, E |- b : Pattern, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
makeAssocAP :: Admin a -> Pattern a -> Maybe String -> Association a
makeAssocAP (A a) (P p) desc = L $ Relation' a p desc

-- | Creates an association relation between an Admin and Deployment pattern
--
-- E |- a : Admin, E |- b : Deployment, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
makeAssocAD :: Admin a -> Deployment a -> Maybe String -> Association a
makeAssocAD (A a) (D d) desc = L $ Relation' a d desc

-- | Creates an association relation between an Admin and System pattern
--
-- E |- a : Admin, E |- b : System, E |- L'(a,b,L)
-- -----------------------------------------------
-- E |- a linkedTo b : L
makeAssocAS :: Admin a -> System a -> Maybe String -> Association a
makeAssocAS (A a) (S s) desc = L $ Relation' a s desc

-- | Creates an association relation between an Admin and Component pattern
--
-- E |- a : Admin, E |- b : Component, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
makeAssocAC :: Admin a -> Component a -> Maybe String -> Association a
makeAssocAC (A a) (C c) desc = L $ Relation' a c desc

-- | Creates an association relation between an Admin and Implementation pattern
--
-- E |- a : Admin, E |- b : Implementation, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocAI :: Admin a -> Implementation a -> Maybe String -> Association a
makeAssocAI (A a) (I i) desc = L $ Relation' a i desc

-- | Creates an association relation between an Component and Pattern pattern
--
-- E |- a : Component, E |- b : Pattern, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCP :: Component a -> Pattern a -> Maybe String -> Association a
makeAssocCP (C c) (P p) desc = L $ Relation' c p desc

-- | Creates an association relation between an Component and Deployment pattern
--
-- E |- a : Component, E |- b : Deployment, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCD :: Component a -> Deployment a -> Maybe String -> Association a
makeAssocCD (C c) (D d) desc = L $ Relation' c d desc

-- | Creates an association relation between an Component and System pattern
--
-- E |- a : Component, E |- b : System, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCS :: Component a -> System a -> Maybe String -> Association a
makeAssocCS (C c) (S s) desc = L $ Relation' c s desc

-- | Creates an association relation between an Component and Admin pattern
--
-- E |- a : Component, E |- b : Admin, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCA :: Component a -> Admin a -> Maybe String -> Association a
makeAssocCA (C c) (A a) desc = L $ Relation' c a desc

-- | Creates an association relation between an Component and Implementation pattern
--
-- E |- a : Component, E |- b : Implementation, E |- L'(a,b,L)
-- -----------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCI :: Component a -> Implementation a -> Maybe String -> Association a
makeAssocCI (C c) (I i) desc = L $ Relation' c i desc

-- | Creates an association relation between an Implementation and Pattern pattern
--
-- E |- a : Implementation, E |- b : Pattern, E |- L'(a,b,L)
-- ---------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocIP :: Implementation a -> Pattern a -> Maybe String -> Association a
makeAssocIP (I i) (P p) desc = L $ Relation' i p desc

-- | Creates an association relation between an Implementation and Deployment pattern
--
-- E |- a : Implementation, E |- b : Deployment, E |- L'(a,b,L)
-- ------------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocID :: Implementation a -> Deployment a -> Maybe String -> Association a
makeAssocID (I i) (D d) desc = L $ Relation' i d desc

-- | Creates an association relation between an Implementation and System pattern
--
-- E |- a : Implementation, E |- b : System, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocIS :: Implementation a -> System a -> Maybe String -> Association a
makeAssocIS (I i) (S s) desc = L $ Relation' i s desc

-- | Creates an association relation between an Implementation and Admin pattern
--
-- E |- a : Implementation, E |- b : Admin, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocIA :: Implementation a -> Admin a -> Maybe String -> Association a
makeAssocIA (I i) (A a) desc = L $ Relation' i a desc

-- | Creates an association relation between an Implementation and Component pattern
--
-- E |- a : Implementation, E |- b : Component, E |- L'(a,b,L)
-- -----------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocIC :: Implementation a -> Component a -> Maybe String -> Association a
makeAssocIC (I i) (C c) desc = L $ Relation' i c desc

-- | Creates an association relation between an Pattern and Pattern pattern
--
-- E |- a : Pattern, E |- b : Pattern, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
makeAssocPP :: Pattern a -> Pattern a -> Maybe String -> Association a
makeAssocPP (P p) (P q) desc = L $ Relation' p q desc

-- | Creates an association relation between an Deployment and Deployment pattern
--
-- E |- a : Deployment, E |- b : Deployment, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocDD :: Deployment a -> Deployment a -> Maybe String -> Association a
makeAssocDD (D d) (D e) desc = L $ Relation' d e desc

-- | Creates an association relation between an System and System pattern
--
-- E |- a : System, E |- b : System, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
makeAssocSS :: System a -> System a -> Maybe String -> Association a
makeAssocSS (S s) (S t) desc = L $ Relation' s t desc

-- | Creates an association relation between an Admin and Admin pattern
--
-- E |- a : Admin, E |- b : Admin, E |- L'(a,b,L)
-- ----------------------------------------------
-- E |- a linkedTo b : L
makeAssocAA :: Admin a -> Admin a -> Maybe String -> Association a
makeAssocAA (A a) (A b) desc = L $ Relation' a b desc

-- | Creates an association relation between an Component and Component pattern
--
-- E |- a : Component, E |- b : Component, E |- L'(a,b,L)
-- ------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocCC :: Component a -> Component a -> Maybe String -> Association a
makeAssocCC (C c) (C d) desc = L $ Relation' c d desc

-- | Creates an association relation between an Implementation and Implementation pattern
--
-- E |- a : Implementation, E |- b : Implementation, E |- L'(a,b,L)
-- ----------------------------------------------------------------
-- E |- a linkedTo b : L
makeAssocII :: Implementation a -> Implementation a -> Maybe String -> Association a
makeAssocII (I i) (I j) desc = L $ Relation' i j desc


-- --------------------------------------------------------------------- [ EOF ]
