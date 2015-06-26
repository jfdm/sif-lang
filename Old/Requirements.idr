module Sif.Requirements

import Data.Sigma.DList

import GRL.Lang.GLang

%access public
%default total


-- ------------------------------------------------------- [ Requirement Types ]

||| Within the Problem EDSL there are five types of force.
data FTy = FuncTy | UsabTy | ReliTy | PerfTy | SuppTy

instance Show FTy where
  show FuncTy  = "FuncTy"
  show UsabTy  = "UsabTy"
  show ReliTy  = "ReliTy"
  show PerfTy  = "PerfTy"
  show SuppTy  = "SuppTy"

instance Eq FTy where
  (==) FuncTy FuncTy = True
  (==) UsabTy UsabTy = True
  (==) ReliTy ReliTy = True
  (==) PerfTy PerfTy = True
  (==) SuppTy SuppTy = True
  (==) _      _      = False

abstract
data Req : GLang ELEM -> FTy -> Type where
  prim_req : String -> Req e ty

mkRequirement : (fty : FTy) -> (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) fty
mkRequirement _ s _ _ = prim_req s

mkFunctional : (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) FuncTy
mkFunctional s _ _ = prim_req s

mkUsability : (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) UsabTy
mkUsability s _ _ = prim_req s

mkReliability : (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) ReliTy
mkReliability s _ _ = prim_req s

mkPerformance : (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) PerfTy
mkPerformance s _ _ = prim_req s

mkSupportability : (s : String) -> (ety : GElemTy) -> (sval : Maybe SValue) -> Req (mkG ety s sval) SuppTy
mkSupportability s _ _ = prim_req s


private
showReq : Req e ty -> String
showReq {ty} (prim_req desc) = "[" ++ show ty ++ " " ++ show desc ++ "]"

instance Show (Req e ty) where
  show = showReq

private
eqRequire : Req ex xty -> Req ey yty -> Bool
eqRequire {xty} {yty} (prim_req x) (prim_req y) = xty == yty && x == y

instance Eq (Req e ty) where
  (==) = eqRequire

-- ----------------------------------------------------- [ Sub Requirement ADT ]

||| State that the requirement can be broken down into other
||| requirements.
|||
data SubReqLink : GLang STRUCT -> Type where
  MkAndLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x &= ys)
  MkIorLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x |= ys)
  MkXorLink : Req x xty -> DList (GLang ELEM) (\y => Req y yty) ys -> SubReqLink (x X= ys)

instance Show (SubReqLink s) where
  show (MkAndLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkIorLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"
  show (MkXorLink a bs) = "[" ++ show a ++ showDList show bs ++ "]"

eqSRLink : SubReqLink x -> SubReqLink y -> Bool
eqSRLink {x} {y} _ _ = x == y

instance Eq (SubReqLink x) where
  (==) = eqSRLink

-- --------------------------------------------------------- [ IntentLink ADTs ]

||| State the impact or effect that a force has on another force.
data IntentLink : GLang INTENT -> Type where
  MkFImpact : (c : CValue) -> Req x xty -> Req y yty -> IntentLink (x ==> y | c)
  MkFEffect : (c : CValue) -> Req x xty -> Req y yty -> IntentLink (x ~~> y | c)

instance Show (IntentLink i) where
  show (MkFImpact c a b) = "[Req Impacts " ++ show a ++ " " ++ show b ++ "]"
  show (MkFEffect c a b) = "[Req Effects " ++ show a ++ " " ++ show b ++ "]"


-- @TODO Add Eq Instance

eqIntentLink : IntentLink x -> IntentLink y -> Bool
eqIntentLink (MkFImpact xc xa xb) (MkFImpact yc ya yb) = xc == yc && eqRequire xa ya && eqRequire xb yb
eqIntentLink (MkFEffect xc xa xb) (MkFEffect yc ya yb) = xc == yc && eqRequire xa ya && eqRequire xb yb
eqIntentLink _              _                          = False

instance Eq (IntentLink x) where
  (==) = eqIntentLink
-- --------------------------------------------------------------------- [ EOF ]
