module Sif.Pattern

import public Data.Sigma.DList
import public GRL.Lang.GLang
import Data.Vect

%access public
%default total

-- ----------------------------------------------- [ Problems and Requirements ]

-- Encoding of CONTROL | CODE | ACTION ??
-- Encoding of Categories => Security, Access Control, HCI...

-- These internal types are not used. How to use them?

data RTy = FUNC | USAB | RELI | PERF | SUPP
data TTy = ADV  | DIS
data STy = ABSTRACT | CONCRETE

data SifTy = tyREQ | tyTRAIT | tyPROPERTY | tySOLUTION | tyPROBLEM | tyPATTERN

-- ------------------------------------------------------------ [ Requirements ]
private
data InterpRes : SifTy -> Type where
  IReq   : GLang ELEM                       -> InterpRes tyREQ
  IProb  : GLang ELEM -> GModel             -> InterpRes tyPROBLEM
  ITrait : (GLang ELEM, GLang INTENT)       -> InterpRes tyTRAIT
  IProp  : GLang ELEM -> DList GTy GLang es -> InterpRes tyPROPERTY
  ISolt  : GLang ELEM -> DList GTy GLang ss -> InterpRes tySOLUTION
  IPatt  : GModel                           -> InterpRes tyPATTERN

private
interpReq : String -> InterpRes tyREQ
interpReq s = IReq $ MkGoal s Nothing

private
interpProb : String -> List (InterpRes tyREQ) -> InterpRes tyPROBLEM
interpProb s ps = IProb root model
  where
    root : GLang ELEM
    root = MkGoal s Nothing

    cs : List (GLang ELEM)
    cs = map (\(IReq x) => x) ps

    model : GModel
    model = insertMany cs (emptyModel \= root \= (root &= cs) )

private
interpTrait : String
           -> Maybe SValue
           -> CValue
           -> InterpRes tyREQ
           -> InterpRes tyTRAIT
interpTrait s m c (IReq e) = ITrait (node, node ==> e | c)
  where
    node : GLang ELEM
    node = MkTask s m

private
interpProp : String
          -> List (InterpRes tyTRAIT)
          -> InterpRes tyPROPERTY
interpProp s ts = IProp pelem ([pelem, newCS] ++ getProof newES ++ getProof newIS)
  where
    pelem : GLang ELEM
    pelem = MkTask s Nothing

    updateIntent : GLang INTENT -> GLang INTENT
    updateIntent (MkImpacts c a b) = MkImpacts c pelem b
    updateIntent (MkEffects c a b) = MkEffects c pelem b

    newTS : List (GLang ELEM, GLang INTENT)
    newTS = map (\(ITrait (x, y)) => (x,updateIntent y)) ts

    newCS : GLang STRUCT
    newCS = (pelem &= map fst newTS)

    newIS : (is ** DList GTy GLang is)
    newIS = fromList $ map snd newTS

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
    doGet (IProp _ ys) (_ ** res) = (_ ** ys ++ res)

    getDecls : (is ** DList GTy GLang is)
    getDecls = foldr (doGet) (_ ** DList.Nil)  ps

private
interpPatt : String
          -> InterpRes tyPROBLEM
          -> InterpRes tySOLUTION
          -> InterpRes tyPATTERN
interpPatt s (IProb rP m) (ISolt rS is) = IPatt (model \= (root &= [rP,rS]))
  where
    root : GLang ELEM
    root = MkGoal s Nothing

    model : GModel
    model = DList.foldr (insert) m is

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

  priv__mkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : Maybe SValue)
               -> (cval  : CValue)
               -> SifPriv r tyREQ
               -> SifPriv (interpTrait title sval cval r) tyTRAIT

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


mkFunc : String -> Maybe String -> FUNCTIONAL
mkFunc s desc = MkExpr $ priv__mkReq FUNC s desc

mkUsab : String -> Maybe String -> USABILITY
mkUsab s desc = MkExpr $ priv__mkReq USAB s desc

mkReli : String -> Maybe String -> RELIABILITY
mkReli s desc = MkExpr $ priv__mkReq RELI s desc

mkPerf : String -> Maybe String -> PERFORMANCE
mkPerf s desc = MkExpr $ priv__mkReq PERF s desc

mkSupp : String -> Maybe String -> SUPPORTABILITY
mkSupp s desc = MkExpr $ priv__mkReq SUPP s desc

mkProblem : String -> Maybe String -> List (SifExpr tyREQ) -> PROBLEM
mkProblem s d rs = MkExpr $ priv__mkProb s d (getProof $ conv rs)

mkAdv : String -> Maybe String -> Maybe SValue -> CValue -> REQUIREMENT -> ADVANTAGE
mkAdv t d s c (MkExpr r) =
    MkExpr $ priv__mkTrait ADV t d s c r

mkDis : String -> Maybe String -> Maybe SValue -> CValue -> REQUIREMENT -> DISADVANTAGE
mkDis t d s c (MkExpr r) =
    MkExpr $ priv__mkTrait DIS t d s c r

mkProp : String -> Maybe String -> TRAITS -> PROPERTY
mkProp t d ts = MkExpr $ priv__mkProp t d (getProof $ conv ts)

mkSol : String -> Maybe String -> PROPERTIES -> SOLUTION
mkSol s d ps = MkExpr $ priv__mkSolt s d (getProof $ conv ps)

mkPatt : String -> Maybe String -> PROBLEM -> SOLUTION -> PATTERN
mkPatt t d (MkExpr p) (MkExpr s) = MkExpr $ priv__mkPatt t d p s

-- --------------------------------------------------------------------- [ EOF ]
