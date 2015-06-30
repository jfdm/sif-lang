module Sif.Pattern

import public Data.Sigma.DList
import public GRL.Lang.GLang
import Data.Vect

import Debug.Trace

%access public
%default total

-- ----------------------------------------------- [ Problems and Requirements ]

-- Encoding of CONTROL | CODE | ACTION ??
-- Encoding of Categories => Security, Access Control, HCI...

-- These internal types are not used. How to use them?

data RTy = FUNC | USAB | RELI | PERF | SUPP
data TTy = ADV  | DIS
data STy = ABSTRACT | CONCRETE

data SifTy = tyREQ | tyTRAIT | tyPROPERTY | tySOLUTION
           | tyPROBLEM | tyPATTERN | tyTRAITend

-- ------------------------------------------------------------ [ Requirements ]

data InterpRes : SifTy -> Type where
  IReq    : GLang ELEM -> InterpRes tyREQ
  IProb   : GLang ELEM -> GModel               -> InterpRes tyPROBLEM
  ITraitL : GLang ELEM -> CValue               -> InterpRes tyTRAITend
  ITrait  : GLang ELEM -> List (GLang INTENT)  -> InterpRes tyTRAIT
  IProp   : GLang ELEM -> DList GTy GLang es   -> InterpRes tyPROPERTY
  ISolt   : GLang ELEM -> DList GTy GLang ss   -> InterpRes tySOLUTION
  IPatt   : GModel                             -> InterpRes tyPATTERN

private
interpReq : String -> InterpRes tyREQ
interpReq s = IReq root
  where
    root : GLang ELEM
    root = MkGoal s Nothing

private
interpProb : String -> List (InterpRes tyREQ) -> InterpRes tyPROBLEM
interpProb s ps = IProb root (model' \= (root &= cs))
  where
    root : GLang ELEM
    root = MkGoal s Nothing

    cs : List (GLang ELEM)
    cs = map (\(IReq x) => x) ps

    model : GModel
    model = (emptyModel \= root)

    model' : GModel
    model' = insertMany cs model


private
interpTLink : CValue -> InterpRes tyREQ -> InterpRes tyTRAITend
interpTLink c (IReq r) = ITraitL r c

private
interpTrait : String
           -> SValue
           -> List (InterpRes tyTRAITend)
           -> InterpRes tyTRAIT
interpTrait s m es = ITrait node cs
  where
    node : GLang ELEM
    node = MkTask s (Just m)

    cs : List (GLang INTENT)
    cs = map (\(ITraitL r c) => node ==> r | c) es

private
interpProp : String
          -> List (InterpRes tyTRAIT)
          -> InterpRes tyPROPERTY
interpProp s ts = IProp pelem ([pelem] ++ getProof newES ++ getProof newIS ++ [newCS])
  where
    pelem : GLang ELEM
    pelem = MkTask s Nothing

    updateIntent : GLang INTENT -> GLang INTENT
    updateIntent (MkImpacts c a b) = MkImpacts c pelem b
    updateIntent (MkEffects c a b) = MkEffects c pelem b

    newTS : List (GLang ELEM, List (GLang INTENT))
    newTS = map (\(ITrait x ys) => (x, map updateIntent ys)) ts

    newCS : GLang STRUCT
    newCS = (pelem &= map fst newTS)

    newIS : (is ** DList GTy GLang is)
    newIS = fromList $ concat $ map snd newTS

    newES : (es ** DList GTy GLang es)
    newES = fromList $ map fst newTS

private
interpSolt : String
          -> List (InterpRes tyPROPERTY)
          -> InterpRes tySOLUTION
interpSolt s ps = ISolt root ([root, cs] ++ (getProof getDecls))
  where
    root : GLang ELEM
    root = MkTask s Nothing

    cs : GLang STRUCT
    cs = (root &= map (\(IProp x ys) => x) ps)

    doGet : InterpRes tyPROPERTY -> (is ** DList GTy GLang is) -> (xs ** DList GTy GLang xs)
    doGet (IProp x ys) (is ** res) = (_ ** ys ++ res)

    getDecls : (as ** DList GTy GLang as)
    getDecls = foldr (\e,res => doGet e res) (_ ** DList.Nil)  ps

private
interpPatt : String
          -> InterpRes tyPROBLEM
          -> InterpRes tySOLUTION
          -> InterpRes tyPATTERN
interpPatt s (IProb rP m) (ISolt rS is) = IPatt ((model \= root) \= (root &= [rP,rS]))
  where
    root : GLang ELEM
    root = MkGoal s Nothing

    model : GModel
    model = let (_ ** ds) = groupDecls is in DList.foldl (flip $ insert) m (trace (showDList show ds) ds)



-- ----------------------------------------- [ Private Internal Data Structure ]
private
data SifPriv : InterpRes ty -> SifTy -> Type where
  priv__mkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> SifPriv (interpReq t) tyREQ

  priv__mkProb : (title : String)
              -> (desc  : Maybe String)
              -> DList (InterpRes tyREQ) (\x => SifPriv x tyREQ) xs
              -> SifPriv (interpProb title xs) tyPROBLEM

  priv__mkTLink : (cval : CValue)
              -> SifPriv r tyREQ
              -> SifPriv (interpTLink cval r) tyTRAITend

  priv__mkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> DList (InterpRes tyTRAITend) (\x => SifPriv x tyTRAITend) rs
               -> SifPriv (interpTrait title sval rs) tyTRAIT

  priv__mkProp : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyTRAIT) (\x => SifPriv x tyTRAIT) ts
              -> SifPriv (interpProp title ts) tyPROPERTY

  priv__mkSolt : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyPROPERTY) (\x => SifPriv x tyPROPERTY) ps
              -> SifPriv (interpSolt title ps) tySOLUTION

  priv__mkPatt : (title : String)
              -> (desc : Maybe String)
              -> SifPriv p tyPROBLEM
              -> SifPriv s tySOLUTION
              -> SifPriv (interpPatt title p s) tyPATTERN

-- --------------------------------------------------- [ Public Data Structure ]

abstract
data SifExpr : SifTy -> Type where
  MkExpr  : {i : InterpRes ty} -> SifPriv i ty-> SifExpr ty

-- ----------------------------------------------------------- [ Type Synonyms ]

FUNCTIONAL : Type
FUNCTIONAL = SifExpr tyREQ

USABILITY : Type
USABILITY = SifExpr tyREQ

RELIABILITY : Type
RELIABILITY = SifExpr tyREQ

PERFORMANCE : Type
PERFORMANCE = SifExpr tyREQ

SUPPORTABILITY : Type
SUPPORTABILITY = SifExpr tyREQ

REQUIREMENT : Type
REQUIREMENT = SifExpr tyREQ

REQUIREMENTS : Type
REQUIREMENTS = List (SifExpr tyREQ)

PROBLEM : Type
PROBLEM = SifExpr tyPROBLEM

ADVANTAGE : Type
ADVANTAGE = SifExpr tyTRAIT

DISADVANTAGE : Type
DISADVANTAGE = SifExpr tyTRAIT

TRAIT : Type
TRAIT = SifExpr tyTRAIT

TRAITS : Type
TRAITS = List (SifExpr tyTRAIT)

TLINK : Type
TLINK = SifExpr tyTRAITend

TLINKS : Type
TLINKS = List (SifExpr tyTRAITend)

PROPERTY : Type
PROPERTY = SifExpr tyPROPERTY

PROPERTIES : Type
PROPERTIES = List (SifExpr tyPROPERTY)

SOLUTION : Type
SOLUTION = SifExpr tySOLUTION

PATTERN : Type
PATTERN = SifExpr tyPATTERN

-- Public Constructors

private
conv : List (SifExpr ty)
    -> (xs ** DList (InterpRes ty) (\x => SifPriv x ty) xs)
conv xs = fromLDP $ map (\(MkExpr x) => (_ ** x)) xs

mkFunctional : String -> Maybe String -> FUNCTIONAL
mkFunctional s desc = MkExpr $ priv__mkReq FUNC s desc

mkUsability : String -> Maybe String-> USABILITY
mkUsability s desc = MkExpr $ priv__mkReq USAB s desc

mkReliability : String -> Maybe String -> RELIABILITY
mkReliability s desc = MkExpr $ priv__mkReq RELI s desc

mkPerformance : String -> Maybe String -> PERFORMANCE
mkPerformance s desc = MkExpr $ priv__mkReq PERF s desc

mkSupportability : String -> Maybe String -> SUPPORTABILITY
mkSupportability s desc = MkExpr $ priv__mkReq SUPP s desc

private
convR : SifExpr tyREQ -> (r : InterpRes tyREQ ** SifPriv r tyREQ)
convR (MkExpr res) = (_ ** res)

mkProblem : String
         -> Maybe String
         -> (ls : List (SifExpr tyREQ))
         -> {auto prf : NonEmpty ls}
         -> PROBLEM
mkProblem s d rs = MkExpr $ priv__mkProb s d (getProof $ conv rs)

mkLink : CValue -> REQUIREMENT -> TLINK
mkLink c r = MkExpr $ priv__mkTLink c (getProof $ convR r)

mkAdvantage : String
           -> Maybe String
           -> SValue
           -> (ts : TLINKS)
           -> {auto prf : NonEmpty ts}
           -> ADVANTAGE
mkAdvantage t d s rs =
    MkExpr $ priv__mkTrait ADV t d s (getProof $ conv rs)

mkDisadvantage : String
              -> Maybe String
              -> SValue
              -> (ts : TLINKS)
              -> {auto prf : NonEmpty ts}
              -> DISADVANTAGE
mkDisadvantage t d s rs =
    MkExpr $ priv__mkTrait DIS t d s (getProof $ conv rs)

mkProperty : String
          -> Maybe String
          -> (ts : TRAITS)
          -> {auto prf : NonEmpty ts}
          -> PROPERTY
mkProperty t d ts = MkExpr $ priv__mkProp t d (getProof $ conv ts)

mkSolution : String
          -> Maybe String
          -> (ps : PROPERTIES)
          -> {auto prf : NonEmpty ps}
          -> SOLUTION
mkSolution s d ps = MkExpr $ priv__mkSolt s d (getProof $ conv ps)

mkPattern : String -> Maybe String -> PROBLEM -> SOLUTION -> PATTERN
mkPattern t d (MkExpr p) (MkExpr s) = MkExpr $ priv__mkPatt t d p s

getModel : PATTERN -> GModel
getModel (MkExpr p) = doGetModel p
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m

    doGetModel : {i : InterpRes tyPATTERN} -> SifPriv i tyPATTERN -> GModel
    doGetModel {i} _ = extract i

-- --------------------------------------------------------------------- [ EOF ]
