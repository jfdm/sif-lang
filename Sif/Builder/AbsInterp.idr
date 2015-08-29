-- ----------------------------------------------------------- [ AbsInterp.idr ]
-- Module    : AbsInterp.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.AbsInterp

import Data.AVL.Dict
import Data.GraphViz.SimpleDot

import GRL.Lang.GLang
import GRL.Eval

import Edda
import Edda.Reader.Org
import XML.DOM

import Sif.Types
import Sif.Pattern

import Sif.Builder.Utils

-- -------------------------------------------------------------- [ Directives ]

%access private
%default partial

-- --------------------------------------------------- [ Interpretation Result ]

data InterpRes : SifTy -> Type where
  IReq    : GLang ELEM -> InterpRes tyREQ
  IProb   : GLang ELEM -> GModel               -> InterpRes tyPROBLEM
  ITraitL : GLang ELEM -> CValue               -> InterpRes tyAFFECTS
  ITrait  : GLang ELEM -> List (GLang INTENT)  -> InterpRes tyTRAIT
  IProp   : GLang ELEM -> DList GTy GLang es   -> InterpRes tyPROPERTY
  ISolt   : GLang ELEM -> DList GTy GLang ss   -> InterpRes tySOLUTION
  IPatt   : GModel                             -> InterpRes tyPATTERN

interpReq : String -> InterpRes tyREQ
interpReq s = IReq root
  where
    root : GLang ELEM
    root = mkGoal ("Requirement: " ++ s)

interpProb : String -> List (InterpRes tyREQ) -> InterpRes tyPROBLEM
interpProb s ps = IProb root (model' \= (root &= cs))
  where
    root : GLang ELEM
    root = mkGoal ("Problem: " ++ s)

    cs : List (GLang ELEM)
    cs = map (\(IReq x) => x) ps

    model : GModel
    model = (emptyModel \= root)

    model' : GModel
    model' = insertMany cs model

interpTLink : CValue -> InterpRes tyREQ -> InterpRes tyAFFECTS
interpTLink c (IReq r) = ITraitL r c

interpTrait : String
           -> SValue
           -> List (InterpRes tyAFFECTS)
           -> TTy
           -> InterpRes tyTRAIT
interpTrait s m es ty = ITrait node cs
  where
    sVal : SValue -> SValue
    sVal v = case ty of  -- Disadvantages...
               GEN => v
               ADV => v
               DIS => v -- invertEval v

    tVal : String
    tVal = case ty of
             GEN => "Trait General:"
             ADV => "Trait Advantage: "
             DIS => "Trait Disadvantage: "

    node : GLang ELEM
    node = mkSatTask (tVal ++ s) (sVal m)

    cs : List (GLang INTENT)
    cs = map (\(ITraitL r c) => node ==> r | c) es

interpProp : String
          -> List (InterpRes tyTRAIT)
          -> InterpRes tyPROPERTY
interpProp s ts = IProp pelem (Sigma.getProof elems)
  where
    pelem : GLang ELEM
    pelem = mkTask ("Property: " ++ s)

    newTS : List (GLang ELEM, List (GLang INTENT))
    newTS = map (\(ITrait x ys) => (x, ys)) ts

    newCS : GLang STRUCT
    newCS = (pelem &= map fst newTS)

    newIS : (is ** DList GTy GLang is)
    newIS = fromList $ concat $ map snd newTS

    newES : (es ** DList GTy GLang es)
    newES = fromList $ map fst newTS

    elems : (fs ** DList GTy GLang fs)
    elems =  (_ ** [pelem]
                ++ Sigma.getProof newES
                ++ Sigma.getProof newIS
                ++ [newCS])

interpSolt : String
          -> List (InterpRes tyPROPERTY)
          -> InterpRes tySOLUTION
interpSolt s ps = ISolt root (Sigma.getProof elems)
  where
    root : GLang ELEM
    root = mkTask ("Solution: " ++ s)

    cs : GLang STRUCT
    cs = (root &= map (\(IProp x ys) => x) ps)

    doGet : InterpRes tyPROPERTY
         -> (is ** DList GTy GLang is)
         -> (xs ** DList GTy GLang xs)
    doGet (IProp x ys) (is ** res) = (_ ** ys ++ res)

    getDecls : (as ** DList GTy GLang as)
    getDecls = foldr (\e,res => doGet e res) (_ ** DList.Nil)  ps

    elems : (es ** DList GTy GLang es)
    elems = (_ ** [root, cs] ++ (Sigma.getProof getDecls))

interpPatt : InterpRes tyPROBLEM
          -> InterpRes tySOLUTION
          -> InterpRes tyPATTERN
interpPatt (IProb rP m) (ISolt rS is) = IPatt (DList.foldl (flip $ insert) m (getProof $ groupDecls is))


-- ----------------------------------------- [ Private Internal Data Structure ]

data AbsInterpPriv : InterpRes ty -> SifTy -> Type where
  priv__mkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> AbsInterpPriv (interpReq t) tyREQ

  priv__mkProb : (title : String)
              -> (desc  : Maybe String)
              -> DList (InterpRes tyREQ) (\x => AbsInterpPriv x tyREQ) xs
              -> AbsInterpPriv (interpProb title xs) tyPROBLEM

  priv__mkTLink : (cval : CValue)
               -> (req : AbsInterpPriv r tyREQ)
               -> (desc : Maybe String)
               -> AbsInterpPriv (interpTLink cval r) tyAFFECTS

  priv__mkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> DList (InterpRes tyAFFECTS) (\x => AbsInterpPriv x tyAFFECTS) rs
               -> AbsInterpPriv (interpTrait title sval rs ty) tyTRAIT

  priv__mkProp : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyTRAIT) (\x => AbsInterpPriv x tyTRAIT) ts
              -> AbsInterpPriv (interpProp title ts) tyPROPERTY

  priv__mkSolt : (title : String)
              -> (desc : Maybe String)
              -> DList (InterpRes tyPROPERTY) (\x => AbsInterpPriv x tyPROPERTY) ps
              -> AbsInterpPriv (interpSolt title ps) tySOLUTION

  priv__mkPatt : (title : String)
              -> (desc : Maybe String)
              -> AbsInterpPriv p tyPROBLEM
              -> AbsInterpPriv s tySOLUTION
              -> AbsInterpPriv (interpPatt p s) tyPATTERN

covering
getPrivTitle : AbsInterpPriv i ty -> Maybe String
getPrivTitle (priv__mkReq _ t _)       = Just t
getPrivTitle (priv__mkProb t _ _)      = Just t
getPrivTitle (priv__mkTrait _ t _ _ _) = Just t
getPrivTitle (priv__mkProp t _ _)      = Just t
getPrivTitle (priv__mkSolt t _ _)      = Just t
getPrivTitle (priv__mkPatt t _ _ _)    = Just t
getPrivTitle _                         = Nothing

getReqTitle : AbsInterpPriv i tyREQ -> String
getReqTitle (priv__mkReq ty t d) = t
getReqTitle _                    = ""

-- -------------------------------------------------------------------- [ Show ]

covering
showAbsInterpPriv : AbsInterpPriv i ty -> String
showAbsInterpPriv (priv__mkReq ty t d) = with List
    unwords ["   ", show ty, show t]

showAbsInterpPriv (priv__mkProb t d rs) = with List
    unwords [ "  Problem:", show t, "\n"
            , unlines $ mapDList (\x => showAbsInterpPriv x) rs
            , "\n"]

showAbsInterpPriv (priv__mkTLink cval r d) = with List
    unwords ["       ", show cval, (getReqTitle r), "\n"]

showAbsInterpPriv (priv__mkTrait ty t d s rs) = with List
    unwords [
        "     ", show ty, ":" , show t , "is", show s , "\n"
      , concat (mapDList (showAbsInterpPriv) rs)
      , "\n"]

showAbsInterpPriv (priv__mkProp t d ts) = with List
    unwords [
        "     Property: " , t , "\n"
     , concat (mapDList (showAbsInterpPriv) ts)
     , "\n"]

showAbsInterpPriv (priv__mkSolt t d ps) = with List
    unwords [
         "  Solution: " , t , "\n"
      , Foldable.concat (mapDList showAbsInterpPriv ps)
      , "\n"]

showAbsInterpPriv (priv__mkPatt t d p s) = with List
    unwords [
         "Pattern:" , show t , "\n"
      , showAbsInterpPriv p
      , showAbsInterpPriv s]

-- ------------------------------------------------------------------- [ toORG ]

covering
toOrg : AbsInterpPriv i ty -> String
toOrg (priv__mkReq _ t d) = with List
    unwords [
         "** Requirement:", t, "\n\n",
         fromMaybe "" d, "\n"]

toOrg (priv__mkProb t d rs) = with List
    unwords [
        "* Problem:", t, "\n\n",
        fromMaybe "" d, "\n\n",
        (concat $ intersperse "\n" (mapDList (\x => toOrg x) rs)), "\n\n"]

toOrg (priv__mkTLink cval r d) = with List
    unlines [ unwords ["****", concat ["/",show cval, "/"], (getReqTitle r)]
            , fromMaybe "" d, "\n\n"]

toOrg (priv__mkTrait ty t d s rs) = with List
    unwords [
        "*** Trait:", show ty, t , "\n\n"
      , "+ Evaluation Value :: " , show s , "\n\n"
      , fromMaybe "" d , "\n\n"
      , concat (mapDList (toOrg) rs)
      , "\n\n"]

toOrg (priv__mkProp t d ts) = with List
    unwords [
        "** Property: " , t , "\n\n"
     , fromMaybe "" d , "\n\n"
     , concat (mapDList (toOrg) ts)
     , "\n\n"]

toOrg (priv__mkSolt t d ps) = with List
    unwords [
         "* Solution: " , t , "\n\n"
      , fromMaybe "" d , "\n\n"
      , concat (mapDList toOrg ps)
      , "\n\n"]

toOrg (priv__mkPatt t d p s) = with List
    unwords [
         "#+TITLE: " , t , "\n"
      , "#+AUTHOR: Unknown\n"
      , "#+DATE:   Unknown\n"
      , fromMaybe "" d , "\n\n"
      , toOrg p , "\n\n"
      , toOrg s , "\n\n"]

-- --------------------------------------------------------------------- [ XML ]

XEffs : List EFFECT
XEffs = [STATE (Nat, Dict String Nat)]

covering
toXML' : AbsInterpPriv i ty -> Eff (Document ELEMENT) XEffs
toXML' (priv__mkReq ty t d) = do
       let e  = addScore $ mkNDNode (cast ty) t d
       (idGen,_) <- get
       let idVal = cast {to=Int} (S idGen)
       let e' = setAttribute "id" (cast idVal) e
       update (\(idGen,ids) => ((S idGen), insert t (S idGen) ids))
       pure $ e'

toXML' (priv__mkProb t d rs) = do
       let e = addScore $ mkNode "problem"
       -- The following mess is because I could get Effectful HOFs for DList to work.
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML' (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "requirements") rs''
       -- </mess>
       pure $ e
         <++> rNodes
         <++> (addScore $ mkDescNode d)
         <++> ("name" <+=> t)

toXML' (priv__mkTLink cval r d) = do
       (_,ids) <- get
       let id = lookup (getReqTitle r) ids
       let e = case d of
           Nothing => mkNode "affect"
           Just d' => "affect" <+=> d'
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ (setAttribute "cvalue" (cast cval)
              (setAttribute "linksTo" (cast idval) e))

toXML' (priv__mkTrait ty t d s rs) = do
       let e  = addScore $ mkNode (toLower $ show ty)
       let e' = setAttribute "svalue" (cast s) e
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML' (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "affects") rs''
       -- </mess>
       pure $ e'
         <++> (rNodes)
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

toXML' (priv__mkProp t d rs) = do
       let e = addScore $ mkNode "property"
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML' (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "traits") rs''
       -- </mess>
       pure $ e
         <++> rNodes
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

toXML' (priv__mkSolt t d rs) = do
       let e = addScore $ mkNode "solution"
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML' (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "properties") rs''
       -- </mess>
       pure $ e
         <++> (addScore rNodes)
         <++> mkStructure
         <++> mkDynamics
         <++> (mkDescNode d)
         <++> ("name" <+=> t)

toXML' (priv__mkPatt t d p s) = do
       let e = mkNode "pattern"
       pNode <- toXML' p
       sNode <- toXML' s
       pure $ e <++> mkRels
                <++> mkStudies
                <++> (addScore $ mkEmptyNode "evidence")
                <++> sNode
                <++> pNode
                <++> (addScore $ mkEmptyNode "context")
                <++> mkMdata
                <++> (mkDescNode d)
                <++> ("name" <+=> t)

toXML' _ = pure $ mkSimpleElement "bug"

partial
toXML : AbsInterpPriv i ty -> Document DOCUMENT
toXML p = setRoot root $ mkDocument (mkQName "pattern") Nothing
  where
    partial
    root : Document ELEMENT
    root = runPureInit [(Z,Dict.empty)] (toXML' p)

-- -------------------------------------------------------------------- [ Edda ]

toEdda : AbsInterpPriv i tyPATTERN -> Maybe $ Edda PRIME MODEL
toEdda expr =
    case parse parseOrg (toOrg expr) of
      Left  err => Nothing
      Right doc => Just (refineEdda doc)

-- ----------------------------------------------------- [ Instances and Stuff ]


getModel : {x : InterpRes tyPATTERN} -> AbsInterpPriv x tyPATTERN -> GModel
getModel {x} _ = extract x
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m
    extract _         = emptyModel

-- --------------------------------------------------------------------- [ DOT ]

toDot : {i : InterpRes tyPATTERN} -> AbsInterpPriv i tyPATTERN -> SimpleDot GRAPH
toDot p = grlToDot $ getModel p

-- ------------------------------------------------------ [ Builder Definition ]

covering partial
convPriv : {i : InterpRes tyPATTERN}
        -> AbsInterpPriv i tyPATTERN
        -> (fmt : SifOutFormat)
        -> Maybe (convTy fmt)
convPriv p ORG     = Just $ toOrg p
convPriv p XML     = Just $ toXML p
convPriv p DOT     = Just $ toDot p
convPriv p GRL     = Just $ getModel p
convPriv p EDDA    = toEdda p
convPriv p COMPACT = Just $ showAbsInterpPriv p
convPriv p IDRIS   = Nothing

data AbsInterpRep : SifTy -> Type where
  MkWrapper : {i : InterpRes ty} -> AbsInterpPriv i ty -> AbsInterpRep ty

instance Show (AbsInterpPriv i ty) where
  show = showAbsInterpPriv

instance SifRepAPI AbsInterpRep where
  getTitle (MkWrapper i) = getPrivTitle i


  evalPattern (MkWrapper p) =
      case evalModel (getModel p) Nothing of
        BadModel  => Bad
        Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) gs

  toString (MkWrapper p)      = showAbsInterpPriv p
  convTo (MkWrapper   x) fmt  = convPriv x fmt

-- ------------------------------------------------------------- [ The Builder ]

buildReqAbs : RTy -> String -> Maybe String -> REQUIREMENT AbsInterpRep
buildReqAbs ty s d = MkExpr $ MkWrapper $ priv__mkReq ty s d

conv : List (SifExpr AbsInterpRep ty)
    -> (xs ** DList (InterpRes ty) (\x => AbsInterpPriv x ty) xs)
conv xs = fromLDP $ map (\(MkExpr (MkWrapper x)) => (_ ** x)) xs

convR : SifExpr AbsInterpRep tyREQ -> (r : InterpRes tyREQ ** AbsInterpPriv r tyREQ)
convR (MkExpr (MkWrapper res)) = (_ ** res)


buildProblemAbs : String -> Maybe String -> REQUIREMENTS AbsInterpRep -> PROBLEM AbsInterpRep
buildProblemAbs t d rs =
    MkExpr $ MkWrapper $ priv__mkProb t d (Sigma.getProof $ conv rs)

buildAffectAbs : CValue -> REQUIREMENT AbsInterpRep -> Maybe String -> AFFECT AbsInterpRep
buildAffectAbs c r d = MkExpr $ MkWrapper $ priv__mkTLink c (Sigma.getProof $ convR r) d

buildTraitAbs : TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS AbsInterpRep
             -> TRAIT AbsInterpRep
buildTraitAbs ty t d s rs =
    MkExpr $ MkWrapper $ priv__mkTrait ty t d s (Sigma.getProof $ conv rs)


buildPropertyAbs : String -> Maybe String -> TRAITS AbsInterpRep -> PROPERTY AbsInterpRep
buildPropertyAbs t d ts =
    MkExpr $ MkWrapper $ priv__mkProp t d (Sigma.getProof $ conv ts)

buildSolutionAbs : String -> Maybe String -> PROPERTIES AbsInterpRep -> SOLUTION AbsInterpRep
buildSolutionAbs s d ps =
    MkExpr $ MkWrapper $ priv__mkSolt s d (Sigma.getProof $ conv ps)


{-

  buildPattern  : String -> Maybe String -> PROBLEM -> SOLUTION -> PATTERN

-}

buildPatternAbs  : String -> Maybe String -> PROBLEM AbsInterpRep -> SOLUTION AbsInterpRep -> PATTERN AbsInterpRep
buildPatternAbs t d (MkExpr (MkWrapper p)) (MkExpr (MkWrapper s)) = MkExpr $ MkWrapper $ priv__mkPatt t d p s


absInterpBuilder : SifBuilder AbsInterpRep
absInterpBuilder = MkSifBuilder
    buildReqAbs
    buildProblemAbs
    buildAffectAbs
    buildTraitAbs
    buildPropertyAbs
    buildSolutionAbs
    buildPatternAbs

public
backendAbsInterp : SifBackend
backendAbsInterp = MkBackend "interp" absInterpBuilder

-- --------------------------------------------------------------------- [ EOF ]
