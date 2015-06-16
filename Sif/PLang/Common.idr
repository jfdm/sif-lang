module Sif.PLang.Common

-- ------------------------------------------------------------------- [ Types ]
data PTy = COMPONENT | SYSTEM | GENERIC | DEPLOY | ADMIN | CODE

-- -------------------------------------------------------------- [ Predicates ]

data ValidR : PTy -> PTy -> Type where
  RealCC : ValidR COMPONENT COMPONENT
  RealCG : ValidR COMPONENT GENERIC
  RealGG : ValidR GENERIC   GENERIC
  RealIC : ValidR CODE      COMPONENT
  RealIG : ValidR CODE      GENERIC

data ValidI : PTy -> PTy -> Type where
  SpeciSS : ValidI SYSTEM    SYSTEM
  SpeciDS : ValidI DEPLOY    SYSTEM
  SpeciCC : ValidI COMPONENT COMPONENT
  SpeciCG : ValidI COMPONENT GENERIC
  SpeciGG : ValidI GENERIC   GENERIC

data ValidU : PTy -> PTy -> Type where
  UsesCC : ValidU COMPONENT COMPONENT
  UsesCP : ValidU COMPONENT GENERIC
  UsesSS : ValidU SYSTEM    SYSTEM
  UsesSD : ValidU SYSTEM    DEPLOY
  UsesSC : ValidU SYSTEM    COMPONENT
  UsesSA : ValidU SYSTEM    ADMIN
  UsesSP : ValidU SYSTEM    GENERIC
  UsesII : ValidU CODE      CODE
  UsesPP : ValidU GENERIC   GENERIC

data LTy = PATTERN PTy | RELATION | AFFECT | REQUIREMENT | LANG
