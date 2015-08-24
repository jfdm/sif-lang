-- --------------------------------------------------------------- [ Model.idr ]
-- Module    : Model.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Internal Model of Patterns in Sif.
module Sif.Pattern.Model

import Effects
import Effect.State

import Data.Sigma.DList
import Data.AVL.Dict
import GRL.Lang.GLang

import XML.DOM

import Sif.Pattern.Common
import Sif.Pattern.Utils

%access public
%default total

-- ------------------------------------------------------------- [ Interpreter ]

data InterpRes : SifTy -> Type where
  IReq    : GLang ELEM -> InterpRes tyREQ
  IProb   : GLang ELEM -> GModel               -> InterpRes tyPROBLEM
  ITraitL : GLang ELEM -> CValue               -> InterpRes tyTRAITend
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

interpTLink : CValue -> InterpRes tyREQ -> InterpRes tyTRAITend
interpTLink c (IReq r) = ITraitL r c

interpTrait : String
           -> SValue
           -> List (InterpRes tyTRAITend)
           -> TTy
           -> InterpRes tyTRAIT
interpTrait s m es ty = ITrait node cs
  where
    sVal : SValue -> SValue
    sVal v = case ty of
               ADV => v
               DIS => invertEval v

    tVal : String
    tVal = case ty of
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

interpPatt : String
          -> InterpRes tyPROBLEM
          -> InterpRes tySOLUTION
          -> InterpRes tyPATTERN
interpPatt s (IProb rP m) (ISolt rS is) = IPatt model
  where
    model : GModel
    model = let (_ ** ds) = groupDecls is in
        DList.foldl (flip $ insert) m ds


-- ----------------------------------------- [ Private Internal Data Structure ]

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
               -> SifPriv (interpTrait title sval rs ty) tyTRAIT

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

getReqTitle : SifPriv i tyREQ -> String
getReqTitle (priv__mkReq ty t d) = t

-- -------------------------------------------------------------------- [ Show ]

covering
showSifPriv : SifPriv i ty -> String
showSifPriv (priv__mkReq ty t d) = with List
    unwords ["\t\t", show ty, show t]

showSifPriv (priv__mkProb t d rs) = with List
    unwords [ "\t Problem:", show t, "\n"
            , unlines $ mapDList (\x => showSifPriv x) rs
            , "\n"]

showSifPriv (priv__mkTLink cval r) = with List
    unwords ["\t\t\t\t", show cval, (getReqTitle r), "\n"]

showSifPriv (priv__mkTrait ty t d s rs) = with List
    unwords [
        "\t\t\t", show ty, ":" , show t , "is", show s , "\n"
      , concat (mapDList (showSifPriv) rs)
      , "\n"]

showSifPriv (priv__mkProp t d ts) = with List
    unwords [
        "\t\t Property: " , t , "\n"
     , concat (mapDList (showSifPriv) ts)
     , "\n"]

showSifPriv (priv__mkSolt t d ps) = with List
    unwords [
         "\t Solution: " , t , "\n"
      , Foldable.concat (mapDList showSifPriv ps)
      , "\n"]

showSifPriv (priv__mkPatt t d p s) = with List
    unwords [
         "Pattern:" , show t , "\n"
      , showSifPriv p
      , showSifPriv s]

instance Show (SifPriv i ty) where
  show = showSifPriv

covering
toOrg : SifPriv i ty -> String
toOrg (priv__mkReq _ t d) = with List
    unwords [
         "** Requirement:", t, "\n\n",
         fromMaybe "" d, "\n"]

toOrg (priv__mkProb t d rs) = with List
    unwords [
        "* Problem:", t, "\n\n",
        fromMaybe "" d, "\n\n",
        (concat $ intersperse "\n" (mapDList (\x => toOrg x) rs)), "\n\n"]

toOrg (priv__mkTLink cval r) = with List
    unwords ["|", show cval, "|", (getReqTitle r), " |\n"]

toOrg (priv__mkTrait _ t d s rs) = with List
    unwords [
        "*** Trait: " , t , "\n\n"
      , fromMaybe "" d , "\n\n"
      , "+ Evaluation Value :: " , show s , "\n\n"
      , "| Affect Value | Requirement Name |\n"
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
toXML' : SifPriv i ty -> Eff (Document ELEMENT) XEffs
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

toXML' (priv__mkTLink cval r) = do
       (_,ids) <- get
       let id = lookup (getReqTitle r) ids
       let e = mkSimpleElement "affect"
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ (setAttribute "cvalue" (cast cval)
              (setAttribute "linksTo" (cast idval) e))

toXML' (priv__mkTrait ty t d s rs) = do
       let e  = addScore $ mkNode "trait"
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
toXML : SifPriv i ty -> Document ELEMENT
toXML root = runPureInit [(Z,Dict.empty)] (toXML' root)

-- --------------------------------------------------------------------- [ EOF ]
