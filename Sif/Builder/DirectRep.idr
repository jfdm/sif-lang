-- -------------------------------------------------------- [ DirectInterp.idr ]
-- Module    : DirectInterp.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.DirectRep

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

-- ----------------------------------------- [ Private Internal Data Structure ]

data DirectRep : SifTy -> Type where
  DirectMkReq : (ty   : RTy)
             -> (t    : String)
             -> (desc : Maybe String)
             -> DirectRep tyREQ

  DirectMkProb : (title : String)
              -> (desc  : Maybe String)
              -> List (DirectRep tyREQ)
              -> DirectRep tyPROBLEM

  DirectMkAffect : (cval : CValue)
               -> (req : DirectRep tyREQ)
               -> (desc : Maybe String)
               -> DirectRep tyAFFECTS

  DirectMkTrait : (ty : TTy)
               -> (title : String)
               -> (desc  : Maybe String)
               -> (sval  : SValue)
               -> List (DirectRep tyAFFECTS)
               -> DirectRep tyTRAIT

  DirectMkProp : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep tyTRAIT)
              -> DirectRep tyPROPERTY

  DirectMkSolt : (title : String)
              -> (desc : Maybe String)
              -> List (DirectRep tyPROPERTY)
              -> DirectRep tySOLUTION

  DirectMkPatt : (title : String)
              -> (desc : Maybe String)
              -> DirectRep tyPROBLEM
              -> DirectRep tySOLUTION
              -> DirectRep tyPATTERN

covering
getDirectRepTitle : DirectRep ty -> Maybe String
getDirectRepTitle (DirectMkReq _ t _)       = Just t
getDirectRepTitle (DirectMkProb t _ _)      = Just t
getDirectRepTitle (DirectMkTrait _ t _ _ _) = Just t
getDirectRepTitle (DirectMkProp t _ _)      = Just t
getDirectRepTitle (DirectMkSolt t _ _)      = Just t
getDirectRepTitle (DirectMkPatt t _ _ _)    = Just t
getDirectRepTitle _                         = Nothing

-- -------------------------------------------------------------------- [ Show ]

covering
showDirectRep : DirectRep ty -> String
showDirectRep (DirectMkReq ty t d) = with List
    unwords ["   ", show ty, show t]

showDirectRep (DirectMkProb t d rs) = with List
    unwords [ "  Problem:", show t, "\n"
            , unlines $ map (showDirectRep) rs
            , "\n"]

showDirectRep (DirectMkAffect cval r d) = with List
    unwords ["       ", show cval, fromMaybe "Missing" (getDirectRepTitle r), "\n"]

showDirectRep (DirectMkTrait ty t d s rs) = with List
    unwords [
        "     ", show ty, ":" , show t , "is", show s , "\n"
      , concat (map (showDirectRep) rs)
      , "\n"]

showDirectRep (DirectMkProp t d ts) = with List
    unwords [
        "     Property: " , t , "\n"
     , concat (map (showDirectRep) ts)
     , "\n"]

showDirectRep (DirectMkSolt t d ps) = with List
    unwords [
         "  Solution: " , t , "\n"
      , Foldable.concat (map showDirectRep ps)
      , "\n"]

showDirectRep (DirectMkPatt t d p s) = with List
    unwords [
         "Pattern:" , show t , "\n"
      , showDirectRep p
      , showDirectRep s]

-- ------------------------------------------------------------------- [ toORG ]

covering
toOrg : DirectRep ty -> String
toOrg (DirectMkReq _ t d) = with List
    unwords [
         "** Requirement:", t, "\n\n",
         fromMaybe "" d, "\n"]

toOrg (DirectMkProb t d rs) = with List
    unwords [
        "* Problem:", t, "\n\n",
        fromMaybe "" d, "\n\n",
        (concat $ intersperse "\n" (map (\x => toOrg x) rs)), "\n\n"]

toOrg (DirectMkAffect cval r d) = with List
    unlines [ unwords ["****", concat ["/",show cval, "/"], fromMaybe "" (getDirectRepTitle r)]
            , fromMaybe "" d, "\n\n"]

toOrg (DirectMkTrait ty t d s rs) = with List
    unwords [
        "*** Trait:", show ty, t , "\n\n"
      , "+ Evaluation Value :: " , show s , "\n\n"
      , fromMaybe "" d , "\n\n"
      , concat (map (toOrg) rs)
      , "\n\n"]

toOrg (DirectMkProp t d ts) = with List
    unwords [
        "** Property: " , t , "\n\n"
     , fromMaybe "" d , "\n\n"
     , concat (map (toOrg) ts)
     , "\n\n"]

toOrg (DirectMkSolt t d ps) = with List
    unwords [
         "* Solution: " , t , "\n\n"
      , fromMaybe "" d , "\n\n"
      , concat (map toOrg ps)
      , "\n\n"]

toOrg (DirectMkPatt t d p s) = with List
    unwords [
         "#+TITLE: " , t , "\n"
      , "#+AUTHOR: Unknown\n"
      , "#+DATE:   Unknown\n"
      , fromMaybe "" d , "\n\n"
      , toOrg p , "\n\n"
      , toOrg s , "\n\n"]

-- --------------------------------------------------------------------- [ XML ]

{-
XEffs : List EFFECT
XEffs = [STATE (Nat, Dict String Nat)]

covering
toXML' : DirectRep ty -> Eff (Document ELEMENT) XEffs
toXML' (DirectMkReq ty t d) = do
       let e  = addScore $ mkNDNode (cast ty) t d
       (idGen,_) <- get
       let idVal = cast {to=Int} (S idGen)
       let e' = setAttribute "id" (cast idVal) e
       update (\(idGen,ids) => ((S idGen), insert t (S idGen) ids))
       pure $ e'

toXML' (DirectMkProb t d rs) = do
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

toXML' (DirectMkAffect cval r d) = do
       (_,ids) <- get
       let id = lookup (getReqTitle r) ids
       let e = case d of
           Nothing => mkNode "affect"
           Just d' => "affect" <+=> d'
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ (setAttribute "cvalue" (cast cval)
              (setAttribute "linksTo" (cast idval) e))

toXML' (DirectMkTrait ty t d s rs) = do
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

toXML' (DirectMkProp t d rs) = do
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

toXML' (DirectMkSolt t d rs) = do
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

toXML' (DirectMkPatt t d p s) = do
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
toXML : DirectRep ty -> Document DOCUMENT
toXML p = setRoot root $ mkDocument (mkQName "pattern") Nothing
  where
    partial
    root : Document ELEMENT
    root = runPureInit [(Z,Dict.empty)] (toXML' p)
-}
-- -------------------------------------------------------------------- [ Edda ]

toEdda : DirectRep tyPATTERN -> Maybe $ Edda PRIME MODEL
toEdda expr =
    case parse parseOrg (toOrg expr) of
      Left  err => Nothing
      Right doc => Just (refineEdda doc)

-- --------------------------------------------------------------------- [ GRL ]

data InterpRes : SifTy -> Type where
  IReq    : GLang ELEM -> InterpRes tyREQ
  IProb   : GLang ELEM -> GModel               -> InterpRes tyPROBLEM
  IAffect : GLang ELEM -> CValue               -> InterpRes tyAFFECTS
  ITrait  : GLang ELEM -> List (GLang INTENT)  -> InterpRes tyTRAIT
  IProp   : GLang ELEM -> DList GTy GLang es   -> InterpRes tyPROPERTY
  ISolt   : GLang ELEM -> DList GTy GLang ss   -> InterpRes tySOLUTION
  IPatt   : GModel                             -> InterpRes tyPATTERN

covering
toGRL : DirectRep ty -> InterpRes ty
toGRL (DirectMkReq ty t d)  = IReq (mkGoal ("Requirement: " ++ t))

toGRL (DirectMkProb t d rs) = IProb root (model' \= (root &= cs))
  where
    root : GLang ELEM
    root = mkGoal ("Problem: " ++ t)

    cs' : List (InterpRes tyREQ)
    cs' = map toGRL rs

    cs : List (GLang ELEM)
    cs = map (\(IReq x) => x) cs'

    model : GModel
    model = (emptyModel \= root)

    model' : GModel
    model' = insertMany cs model

toGRL (DirectMkAffect cval r d) = IAffect (req $ toGRL r) cval
  where
    req : InterpRes tyREQ -> GLang ELEM
    req (IReq x) = x

toGRL (DirectMkTrait ty t d s rs) = ITrait node cs
  where
    tVal : String
    tVal = case ty of
             GEN => "Trait General:"
             ADV => "Trait Advantage: "
             DIS => "Trait Disadvantage: "

    node : GLang ELEM
    node = mkSatTask (tVal ++ t) s

    es' : List (InterpRes tyAFFECTS)
    es' = map toGRL rs

    cs : List (GLang INTENT)
    cs = map (\(IAffect r c) => node ==> r | c) es'

toGRL (DirectMkProp t d ts) = IProp pelem (Sigma.getProof elems)
  where
    pelem : GLang ELEM
    pelem = mkTask ("Property: " ++ t)

    ts' : List (InterpRes tyTRAIT)
    ts' = map toGRL ts

    newTS : List (GLang ELEM, List (GLang INTENT))
    newTS = map (\(ITrait x ys) => (x, ys)) ts'

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

toGRL (DirectMkSolt t d ps) = ISolt root (Sigma.getProof elems)
  where
    root : GLang ELEM
    root = mkTask ("Solution: " ++ t)

    ps' : List (InterpRes tyPROPERTY)
    ps' = map toGRL ps

    cs : GLang STRUCT
    cs = (root &= map (\(IProp x ys) => x) ps')

    doGet : InterpRes tyPROPERTY
         -> (is ** DList GTy GLang is)
         -> (xs ** DList GTy GLang xs)
    doGet (IProp x ys) (is ** res) = (_ ** ys ++ res)

    getDecls : (as ** DList GTy GLang as)
    getDecls = foldr (\e,res => doGet e res) (_ ** DList.Nil)  ps'

    elems : (es ** DList GTy GLang es)
    elems = (_ ** [root, cs] ++ (Sigma.getProof getDecls))

toGRL (DirectMkPatt t d p s) = IPatt $ mkModel (toGRL p) (toGRL s)
  where
    mkModel : InterpRes tyPROBLEM -> InterpRes tySOLUTION -> GModel
    mkModel (IProb rP m) (ISolt rS is) = (DList.foldl (flip $ insert) m (getProof $ groupDecls is))

-- ----------------------------------------------------- [ Instances and Stuff ]

-- --------------------------------------------------------------------- [ DOT ]

toDot : DirectRep tyPATTERN -> SimpleDot GRAPH
toDot p = grlToDot $ extract (toGRL p)
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m
    extract _         = emptyModel

-- ------------------------------------------------------ [ Builder Definition ]

covering partial
convPriv : DirectRep tyPATTERN
        -> (fmt : SifOutFormat)
        -> Maybe (convTy fmt)
convPriv p ORG     = Just $ toOrg p
convPriv p XML     = Nothing -- Just $ toXML p
convPriv p DOT     = Just $ toDot p
convPriv p GRL     = Just $ (\(IPatt m) => m) (toGRL p)
convPriv p EDDA    = toEdda p
convPriv p COMPACT = Just $ showDirectRep p
convPriv p IDRIS   = Nothing

instance Show (DirectRep ty) where
  show = showDirectRep

instance SifRepAPI DirectRep where
  getTitle = getDirectRepTitle

  evalPattern p = let (IPatt p') = toGRL p  in
      case evalModel p' Nothing of
        BadModel  => Bad
        Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) gs

  toString p     = showDirectRep p
  convTo   x fmt = convPriv x fmt

-- ------------------------------------------------------------- [ The Builder ]

conv : SifExpr DirectRep ty -> DirectRep ty
conv (MkExpr x) = x

buildReqDir : RTy -> String -> Maybe String -> REQUIREMENT DirectRep
buildReqDir ty s d = MkExpr $ DirectMkReq ty s d

buildProblemDir : String -> Maybe String -> REQUIREMENTS DirectRep -> PROBLEM DirectRep
buildProblemDir t d rs =
    MkExpr $ DirectMkProb t d (map conv rs)

buildAffectDir : CValue -> REQUIREMENT DirectRep -> Maybe String -> AFFECT DirectRep
buildAffectDir c r d = MkExpr $ DirectMkAffect c (conv r) d

buildTraitDir : TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS DirectRep
             -> TRAIT DirectRep
buildTraitDir ty t d s rs =
    MkExpr $ DirectMkTrait ty t d s (map conv rs)


buildPropertyDir : String -> Maybe String -> TRAITS DirectRep -> PROPERTY DirectRep
buildPropertyDir t d ts =
    MkExpr $ DirectMkProp t d (map conv ts)

buildSolutionDir : String -> Maybe String -> PROPERTIES DirectRep -> SOLUTION DirectRep
buildSolutionDir s d ps =
    MkExpr $ DirectMkSolt s d (map conv ps)

buildPatternDir  : String -> Maybe String -> PROBLEM DirectRep -> SOLUTION DirectRep -> PATTERN DirectRep
buildPatternDir t d p s= MkExpr $ DirectMkPatt t d (conv p) (conv s)

directBuilder : SifBuilder DirectRep
directBuilder = MkSifBuilder
    buildReqDir
    buildProblemDir
    buildAffectDir
    buildTraitDir
    buildPropertyDir
    buildSolutionDir
    buildPatternDir

public
backendDirectRep : SifBackend
backendDirectRep = MkBackend "direct" directBuilder

-- --------------------------------------------------------------------- [ EOF ]
