-- -------------------------------------------------------------- [ PModel.idr ]
-- Module    : PModel.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.PModel

import public GRL.Common
import public GRL.IR
import public GRL.Model
import public GRL.Builder
import public GRL.Pretty

%access export
%default total

-- ------------------------------------------------------------------- [ Types ]

public export
data FTy = FuncTy | UsabTy  | ReliTy | PerfTy | SuppTy

public export
data ATy = CodeTy | ControlTy | ActionTy

public export
data LTy = ActLink | AffLink

public export
data PMTy = Req FTy | Act ATy | Link LTy | Sub GStructTy

-- ------------------------------------------------------- [ Meta Typing Rules ]

public export
data ValidLink : LTy -> PMTy -> PMTy -> Type where
  RAct : ValidLink ty (Req x) (Req y)
  MAct : ValidLink ty (Act x) (Req y)
  AAct : ValidLink ty (Act y) (Act y)

public export
data ValidDecomp : PMTy -> PMTy -> Type where
  SubR : ValidDecomp (Req x) (Req y)
  SubM : ValidDecomp (Act x) (Act y)

public export
data ValidDecomps : PMTy -> List PMTy -> Type where
  Nil  : ValidDecomps x Nil
  (::) : (x : PMTy)
      -> (y : PMTy)
      -> {auto prf : ValidDecomp x y}
      -> ValidDecomps x ys
      -> ValidDecomps z (y::ys)

-- ------------------------------------------ [ Abstract Syntax & Typing Rules ]

public export
data PModel : PMTy -> GTy -> Type where
  MkReq    : (ty : FTy) -> String -> PModel (Req ty) ELEM
  MkAction : (ty : ATy) -> String -> Maybe SValue -> PModel (Act ty) ELEM
  MkLink   : (ty : LTy)
          -> CValue
          -> PModel x ELEM
          -> PModel y ELEM
          -> {auto prf : ValidLink ty x y}
          -> PModel (Link ty) INTENT
  MkSub    : (ty : GStructTy)
          -> PModel x ELEM
          -> DList PMTy (\y => PModel y ELEM) ys
          -> {auto prf : ValidDecomps x ys}
          -> PModel (Sub ty) STRUCT

-- ----------------------------------------------------- [ Typing Type-Aliases ]

public export
FUNCTIONAL : Type
FUNCTIONAL = PModel (Req FuncTy) ELEM

public export
USABILITY : Type
USABILITY = PModel (Req UsabTy) ELEM

public export
RELIABILITY : Type
RELIABILITY = PModel (Req ReliTy) ELEM

public export
PERFORMANCE : Type
PERFORMANCE = PModel (Req PerfTy) ELEM

public export
SUPPORTABILITY : Type
SUPPORTABILITY = PModel (Req SuppTy) ELEM

public export
CODEACTION : Type
CODEACTION = PModel (Act CodeTy) ELEM

public export
CONTROL : Type
CONTROL = PModel (Act ControlTy) ELEM

public export
ACTION : Type
ACTION = PModel (Act ActionTy) ELEM

-- ---------------------------------------------------- [ Element Constructors ]

mkFunctional : String -> FUNCTIONAL
mkFunctional = MkReq FuncTy

mkUsability : String -> USABILITY
mkUsability = MkReq UsabTy

mkReliability : String -> RELIABILITY
mkReliability = MkReq ReliTy

mkPerformance : String -> PERFORMANCE
mkPerformance = MkReq PerfTy

mkSupportability : String -> SUPPORTABILITY
mkSupportability = MkReq SuppTy

mkControl : String -> Maybe SValue -> CONTROL
mkControl = MkAction ControlTy

mkCode : String -> Maybe SValue -> CODEACTION
mkCode = MkAction CodeTy

mkAction : String -> Maybe SValue -> ACTION
mkAction = MkAction ActionTy

-- -------------------------------------------------------- [ Operator Aliases ]

syntax [a] "==>" [b] "|" [c] = MkLink ActLink c a b
syntax [a] "~~>" [b] "|" [c] = MkLink AffLink c a b
syntax [a] "&=" [bs] = MkSub ANDty a bs
syntax [a] "X=" [bs] = MkSub XORty a bs
syntax [a] "|=" [bs] = MkSub IORty a bs

-- ------------------------------------------------------------- [ Interpreter ]

GRL (\ty => PModel x ty) where
  mkElem (MkReq ty  t)        = Elem GOALty t Nothing
  mkElem (MkAction ty t sval) = Elem TASKty t sval

  mkIntent (MkLink AffLink c a b) = ILink AFFECTSty c (mkElem a) (mkElem b)
  mkIntent (MkLink ActLink c a b) = ILink IMPACTSty  c (mkElem a) (mkElem b)

  mkStruct (MkSub ty x ys) = SLink ty (mkElem x) (mapDList (mkElem) ys)


-- --------------------------------------------------------------------- [ EOF ]
