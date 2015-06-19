module Sif.PLang.Common

%access public
%default total

-- ------------------------------------------------------------------- [ Types ]
data PTy = CompTy | SysTy | GenTy | DeployTy | AdminTy | CodeTy

data RTy = FuncTy | UsabTy | ReliTy | PerfTy | SuppTy
-- -------------------------------------------------------------- [ Predicates ]

data ValidR : PTy -> PTy -> Type where
  RealCC : ValidR CompTy CompTy
  RealCG : ValidR CompTy GenTy
  RealGG : ValidR GenTy   GenTy
  RealIC : ValidR CodeTy      CompTy
  RealIG : ValidR CodeTy      GenTy

data ValidI : PTy -> PTy -> Type where
  SpeciSS : ValidI SysTy    SysTy
  SpeciDS : ValidI DeployTy    SysTy
  SpeciCC : ValidI CompTy CompTy
  SpeciCG : ValidI CompTy GenTy
  SpeciGG : ValidI GenTy   GenTy

data ValidU : PTy -> PTy -> Type where
  UsesCC : ValidU CompTy CompTy
  UsesCP : ValidU CompTy GenTy
  UsesSS : ValidU SysTy    SysTy
  UsesSD : ValidU SysTy    DeployTy
  UsesSC : ValidU SysTy    CompTy
  UsesSA : ValidU SysTy    AdminTy
  UsesSP : ValidU SysTy    GenTy
  UsesII : ValidU CodeTy      CodeTy
  UsesPP : ValidU GenTy   GenTy

-- --------------------------------------------------------------------- [ EOF ]
