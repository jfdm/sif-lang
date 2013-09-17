-- | Smart Constructors/Inference Rules for Association between patterns.
module TypeSystem.RulesAssoc where

import TypeSystem.Types

-- | Creates an association relation between an Pattern and Deployment pattern
--
-- E |- a : Pattern, E |- b : Deployment, E |- L'(a,b,L)
-- -----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPD :: Pattern a -> Deployment a -> Maybe String -> Association a
mkAssocPD (P p) (D d) desc = L $ Relation' p d desc

-- | Creates an association relation between an Pattern and System pattern
--
-- E |- a : Pattern, E |- b : System, E |- L'(a,b,L)
-- -------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPS :: Pattern a -> System a -> Maybe String -> Association a
mkAssocPS (P p) (S s) desc = L $ Relation' p s desc

-- | Creates an association relation between an Pattern and Admin pattern
--
-- E |- a : Pattern, E |- b : Admin, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPA :: Pattern a -> Admin a -> Maybe String -> Association a
mkAssocPA (P p) (A a) desc = L $ Relation' p a desc

-- | Creates an association relation between an Pattern and Component pattern
--
-- E |- a : Pattern, E |- b : Component, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPC :: Pattern a -> Component a -> Maybe String -> Association a
mkAssocPC (P p) (C c) desc = L $ Relation' p c desc

-- | Creates an association relation between an Pattern and Implementation pattern
--
-- E |- a : Pattern, E |- b : Implementation, E |- L'(a,b,L)
-- ---------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPI :: Pattern a -> Implementation a -> Maybe String -> Association a
mkAssocPI (P p) (I i) desc = L $ Relation' p i desc

-- | Creates an association relation between an Deployment and Pattern pattern
--
-- E |- a : Deployment, E |- b : Pattern, E |- L'(a,b,L)
-- -----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDP :: Deployment a -> Pattern a -> Maybe String -> Association a
mkAssocDP (D d) (P p) desc = L $ Relation' d p desc

-- | Creates an association relation between an Deployment and System pattern
--
-- E |- a : Deployment, E |- b : System, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDS :: Deployment a -> System a -> Maybe String -> Association a
mkAssocDS (D d) (S s) desc = L $ Relation' d s desc

-- | Creates an association relation between an Deployment and Admin pattern
--
-- E |- a : Deployment, E |- b : Admin, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDA :: Deployment a -> Admin a -> Maybe String -> Association a
mkAssocDA (D d) (A a) desc = L $ Relation' d a desc

-- | Creates an association relation between an Deployment and Component pattern
--
-- E |- a : Deployment, E |- b : Component, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDC :: Deployment a -> Component a -> Maybe String -> Association a
mkAssocDC (D d) (C c) desc = L $ Relation' d c desc

-- | Creates an association relation between an Deployment and Implementation pattern
--
-- E |- a : Deployment, E |- b : Implementation, E |- L'(a,b,L)
-- ------------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDI :: Deployment a -> Implementation a -> Maybe String -> Association a
mkAssocDI (D d) (I i) desc = L $ Relation' d i desc

-- | Creates an association relation between an System and Pattern pattern
--
-- E |- a : System, E |- b : Pattern, E |- L'(a,b,L)
-- -------------------------------------------------
-- E |- a linkedTo b : L
mkAssocSP :: System a -> Pattern a -> Maybe String -> Association a
mkAssocSP (S s) (P p) desc = L $ Relation' s p desc

-- | Creates an association relation between an System and Deployment pattern
--
-- E |- a : System, E |- b : Deployment, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocSD :: System a -> Deployment a -> Maybe String -> Association a
mkAssocSD (S s) (D d) desc = L $ Relation' s d desc

-- | Creates an association relation between an System and Admin pattern
--
-- E |- a : System, E |- b : Admin, E |- L'(a,b,L)
-- -----------------------------------------------
-- E |- a linkedTo b : L
mkAssocSA :: System a -> Admin a -> Maybe String -> Association a
mkAssocSA (S s) (A a) desc = L $ Relation' s a desc

-- | Creates an association relation between an System and Component pattern
--
-- E |- a : System, E |- b : Component, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
mkAssocSC :: System a -> Component a -> Maybe String -> Association a
mkAssocSC (S s) (C c) desc = L $ Relation' s c desc

-- | Creates an association relation between an System and Implementation pattern
--
-- E |- a : System, E |- b : Implementation, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocSI :: System a -> Implementation a -> Maybe String -> Association a
mkAssocSI (S s) (I i) desc = L $ Relation' s i desc

-- | Creates an association relation between an Admin and Pattern pattern
--
-- E |- a : Admin, E |- b : Pattern, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
mkAssocAP :: Admin a -> Pattern a -> Maybe String -> Association a
mkAssocAP (A a) (P p) desc = L $ Relation' a p desc

-- | Creates an association relation between an Admin and Deployment pattern
--
-- E |- a : Admin, E |- b : Deployment, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
mkAssocAD :: Admin a -> Deployment a -> Maybe String -> Association a
mkAssocAD (A a) (D d) desc = L $ Relation' a d desc

-- | Creates an association relation between an Admin and System pattern
--
-- E |- a : Admin, E |- b : System, E |- L'(a,b,L)
-- -----------------------------------------------
-- E |- a linkedTo b : L
mkAssocAS :: Admin a -> System a -> Maybe String -> Association a
mkAssocAS (A a) (S s) desc = L $ Relation' a s desc

-- | Creates an association relation between an Admin and Component pattern
--
-- E |- a : Admin, E |- b : Component, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
mkAssocAC :: Admin a -> Component a -> Maybe String -> Association a
mkAssocAC (A a) (C c) desc = L $ Relation' a c desc

-- | Creates an association relation between an Admin and Implementation pattern
--
-- E |- a : Admin, E |- b : Implementation, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocAI :: Admin a -> Implementation a -> Maybe String -> Association a
mkAssocAI (A a) (I i) desc = L $ Relation' a i desc

-- | Creates an association relation between an Component and Pattern pattern
--
-- E |- a : Component, E |- b : Pattern, E |- L'(a,b,L)
-- ----------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCP :: Component a -> Pattern a -> Maybe String -> Association a
mkAssocCP (C c) (P p) desc = L $ Relation' c p desc

-- | Creates an association relation between an Component and Deployment pattern
--
-- E |- a : Component, E |- b : Deployment, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCD :: Component a -> Deployment a -> Maybe String -> Association a
mkAssocCD (C c) (D d) desc = L $ Relation' c d desc

-- | Creates an association relation between an Component and System pattern
--
-- E |- a : Component, E |- b : System, E |- L'(a,b,L)
-- ---------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCS :: Component a -> System a -> Maybe String -> Association a
mkAssocCS (C c) (S s) desc = L $ Relation' c s desc

-- | Creates an association relation between an Component and Admin pattern
--
-- E |- a : Component, E |- b : Admin, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCA :: Component a -> Admin a -> Maybe String -> Association a
mkAssocCA (C c) (A a) desc = L $ Relation' c a desc

-- | Creates an association relation between an Component and Implementation pattern
--
-- E |- a : Component, E |- b : Implementation, E |- L'(a,b,L)
-- -----------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCI :: Component a -> Implementation a -> Maybe String -> Association a
mkAssocCI (C c) (I i) desc = L $ Relation' c i desc

-- | Creates an association relation between an Implementation and Pattern pattern
--
-- E |- a : Implementation, E |- b : Pattern, E |- L'(a,b,L)
-- ---------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocIP :: Implementation a -> Pattern a -> Maybe String -> Association a
mkAssocIP (I i) (P p) desc = L $ Relation' i p desc

-- | Creates an association relation between an Implementation and Deployment pattern
--
-- E |- a : Implementation, E |- b : Deployment, E |- L'(a,b,L)
-- ------------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocID :: Implementation a -> Deployment a -> Maybe String -> Association a
mkAssocID (I i) (D d) desc = L $ Relation' i d desc

-- | Creates an association relation between an Implementation and System pattern
--
-- E |- a : Implementation, E |- b : System, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocIS :: Implementation a -> System a -> Maybe String -> Association a
mkAssocIS (I i) (S s) desc = L $ Relation' i s desc

-- | Creates an association relation between an Implementation and Admin pattern
--
-- E |- a : Implementation, E |- b : Admin, E |- L'(a,b,L)
-- -------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocIA :: Implementation a -> Admin a -> Maybe String -> Association a
mkAssocIA (I i) (A a) desc = L $ Relation' i a desc

-- | Creates an association relation between an Implementation and Component pattern
--
-- E |- a : Implementation, E |- b : Component, E |- L'(a,b,L)
-- -----------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocIC :: Implementation a -> Component a -> Maybe String -> Association a
mkAssocIC (I i) (C c) desc = L $ Relation' i c desc

-- | Creates an association relation between an Pattern and Pattern pattern
--
-- E |- a : Pattern, E |- b : Pattern, E |- L'(a,b,L)
-- --------------------------------------------------
-- E |- a linkedTo b : L
mkAssocPP :: Pattern a -> Pattern a -> Maybe String -> Association a
mkAssocPP (P p) (P q) desc = L $ Relation' p q desc

-- | Creates an association relation between an Deployment and Deployment pattern
--
-- E |- a : Deployment, E |- b : Deployment, E |- L'(a,b,L)
-- --------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocDD :: Deployment a -> Deployment a -> Maybe String -> Association a
mkAssocDD (D d) (D e) desc = L $ Relation' d e desc

-- | Creates an association relation between an System and System pattern
--
-- E |- a : System, E |- b : System, E |- L'(a,b,L)
-- ------------------------------------------------
-- E |- a linkedTo b : L
mkAssocSS :: System a -> System a -> Maybe String -> Association a
mkAssocSS (S s) (S t) desc = L $ Relation' s t desc

-- | Creates an association relation between an Admin and Admin pattern
--
-- E |- a : Admin, E |- b : Admin, E |- L'(a,b,L)
-- ----------------------------------------------
-- E |- a linkedTo b : L
mkAssocAA :: Admin a -> Admin a -> Maybe String -> Association a
mkAssocAA (A a) (A b) desc = L $ Relation' a b desc

-- | Creates an association relation between an Component and Component pattern
--
-- E |- a : Component, E |- b : Component, E |- L'(a,b,L)
-- ------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocCC :: Component a -> Component a -> Maybe String -> Association a
mkAssocCC (C c) (C d) desc = L $ Relation' c d desc

-- | Creates an association relation between an Implementation and Implementation pattern
--
-- E |- a : Implementation, E |- b : Implementation, E |- L'(a,b,L)
-- ----------------------------------------------------------------
-- E |- a linkedTo b : L
mkAssocII :: Implementation a -> Implementation a -> Maybe String -> Association a
mkAssocII (I i) (I j) desc = L $ Relation' i j desc


-- --------------------------------------------------------------------- [ EOF ]
