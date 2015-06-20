||| An EDSL for shallow specification of pattern languages.
|||
||| A shallow specification is one in which only problems are
||| specified and their combinations controlled through typing.  This
||| language is intended to be used for specifying languages only.
||| Evaluating languages must make use of the deep version.
module Sif.PLang

import public GRL.Common
import public GRL.IR
import public GRL.Model
import public GRL.Builder
import public GRL.Pretty

%access public
%default total

-- ------------------------------------------------------------------- [ Types ]

data PTy = CompTy | SysTy | GenTy | DeployTy | AdminTy | CodeTy

data RTy = FuncTy | UsabTy | ReliTy | PerfTy | SuppTy

data LTy = PATTERN PTy | HASREQ | RELATION | AFFECT | REQUIREMENT RTy | LANG

-- -------------------------------------------------------------- [ Predicates ]

data ValidR : PTy -> PTy -> Type where
  RealCC : ValidR CompTy CompTy
  RealCG : ValidR CompTy GenTy
  RealGG : ValidR GenTy  GenTy
  RealIC : ValidR CodeTy CompTy
  RealIG : ValidR CodeTy GenTy

data ValidI : PTy -> PTy -> Type where
  SpeciSS : ValidI SysTy    SysTy
  SpeciDS : ValidI DeployTy SysTy
  SpeciCC : ValidI CompTy   CompTy
  SpeciCG : ValidI CompTy   GenTy
  SpeciGG : ValidI GenTy    GenTy

data ValidU : PTy -> PTy -> Type where
  UsesCC : ValidU CompTy CompTy
  UsesCP : ValidU CompTy GenTy
  UsesSS : ValidU SysTy  SysTy
  UsesSD : ValidU SysTy  DeployTy
  UsesSC : ValidU SysTy  CompTy
  UsesSA : ValidU SysTy  AdminTy
  UsesSP : ValidU SysTy  GenTy
  UsesII : ValidU CodeTy CodeTy
  UsesPP : ValidU GenTy  GenTy

-- -------------------------------------------------------------- [ Definition ]

data PLang : LTy -> GTy -> Type where

  Functional     : String -> PLang (REQUIREMENT FuncTy) ELEM
  Usability      : String -> PLang (REQUIREMENT UsabTy) ELEM
  Reliability    : String -> PLang (REQUIREMENT ReliTy) ELEM
  Performance    : String -> PLang (REQUIREMENT PerfTy) ELEM
  Supportability : String -> PLang (REQUIREMENT SuppTy) ELEM

  Component : String -> PLang (PATTERN CompTy)   ELEM
  System    : String -> PLang (PATTERN SysTy)    ELEM
  Generic   : String -> PLang (PATTERN GenTy)    ELEM
  Deploy    : String -> PLang (PATTERN DeployTy) ELEM
  Admin     : String -> PLang (PATTERN AdminTy)  ELEM
  Code      : String -> PLang (PATTERN CodeTy)   ELEM

  Provides : CValue
          -> PLang (PATTERN pty)     ELEM
          -> PLang (REQUIREMENT rty) ELEM
          -> PLang HASREQ            INTENT

  Affects  : CValue
          -> PLang (PATTERN pty)     ELEM
          -> PLang (REQUIREMENT rty) ELEM
          -> PLang AFFECT            INTENT

  LinkedTo : PLang (PATTERN x) ELEM
          -> PLang (PATTERN y) ELEM
          -> PLang RELATION    INTENT

  Implements : PLang (PATTERN x) ELEM
            -> PLang (PATTERN y) ELEM
            -> {auto prf : ValidR x y}
            -> PLang RELATION    INTENT

  Uses : PLang (PATTERN x) ELEM
      -> PLang (PATTERN y) ELEM
      -> {auto prf : ValidU x y}
      -> PLang RELATION    STRUCT

  Specialises : PLang (PATTERN x) ELEM
             -> PLang (PATTERN y) ELEM
             -> {auto prf : ValidI x y}
             -> PLang RELATION    STRUCT

FUNCTIONAL : Type
FUNCTIONAL = PLang (REQUIREMENT FuncTy) ELEM

USABILITY : Type
USABILITY = PLang (REQUIREMENT UsabTy) ELEM

RELIABILITY : Type
RELIABILITY = PLang (REQUIREMENT ReliTy) ELEM

PERFORMANCE : Type
PERFORMANCE = PLang (REQUIREMENT PerfTy) ELEM

SUPPORTABILITY : Type
SUPPORTABILITY = PLang (REQUIREMENT SuppTy) ELEM

COMPONENT : Type
COMPONENT = PLang (PATTERN CompTy) ELEM

SYSTEM : Type
SYSTEM = PLang (PATTERN SysTy) ELEM

GENERIC : Type
GENERIC = PLang (PATTERN GenTy) ELEM

DEPLOY : Type
DEPLOY = PLang (PATTERN DeployTy) ELEM

ADMIN : Type
ADMIN = PLang (PATTERN AdminTy) ELEM

CODE : Type
CODE = PLang (PATTERN CodeTy) ELEM


syntax [a] "o->" [b] "|" [c] = Provides c a b
syntax [a] "o~>" [b] "|" [c] = Affects  c a b

syntax [a] "==>" [b] = LinkedTo    a b
syntax [a] "~~>" [b] = Implements  a b
syntax [a] "]=>" [b] = Uses        a b
syntax [a] "==<" [b] = Specialises a b

instance GRL (\x => PLang ty x) where
  mkGoal (Functional     s) = Elem GOALty s Nothing
  mkGoal (Usability      s) = Elem GOALty s Nothing
  mkGoal (Reliability    s) = Elem GOALty s Nothing
  mkGoal (Performance    s) = Elem GOALty s Nothing
  mkGoal (Supportability s) = Elem GOALty s Nothing

  mkGoal (Component t) = Elem TASKty t Nothing  -- Values populated using strategies
  mkGoal (System    t) = Elem TASKty t Nothing
  mkGoal (Generic   t) = Elem TASKty t Nothing
  mkGoal (Deploy    t) = Elem TASKty t Nothing
  mkGoal (Admin     t) = Elem TASKty t Nothing
  mkGoal (Code      t) = Elem TASKty t Nothing

  mkIntent (Provides c a b) = ILink IMPACTSty c (mkGoal a) (mkGoal b)
  mkIntent (Affects  c a b) = ILink AFFECTSty c (mkGoal a) (mkGoal b)

  mkIntent (LinkedTo a b)    = ILink AFFECTSty UNKNOWN (mkGoal a) (mkGoal b)
  mkIntent (Implements a b)  = ILink IMPACTSty HELPS (mkGoal b) (mkGoal a) -- Swap intentional
  mkStruct (Specialises a b) = SLink ANDty (mkGoal a) [mkGoal b]
  mkStruct (Uses a b)        = SLink ANDty (mkGoal a) [mkGoal b]


-- --------------------------------------------------------------------- [ EOF ]
