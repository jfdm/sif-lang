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
-- import public Data.Sigma.DList.Eff

import Data.AVL.Dict
import GRL.Lang.GLang

import Edda
import Edda.Reader.Org

import XML.DOM

import Sif.Pattern.Utils

import Debug.Trace

%access public
%default total

-- ----------------------------------------------- [ Problems and Requirements ]

-- Encoding of CONTROL | CODE | ACTION ??
-- Encoding of Categories => Security, Access Control, HCI...

-- These internal types are not used. How to use them?

data RTy = FUNC | USAB | RELI | PERF | SUPP

instance Cast RTy String where
  cast FUNC = "functional"
  cast USAB = "usability"
  cast RELI = "reliability"
  cast PERF = "performance"
  cast SUPP = "supportability"

data TTy = ADV  | DIS
data STy = ABSTRACT | CONCRETE

data SifTy = tyREQ     | tyTRAIT   | tyPROPERTY | tySOLUTION
           | tyPROBLEM | tyPATTERN | tyTRAITend

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
    root = MkGoal s Nothing

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

interpTLink : CValue -> InterpRes tyREQ -> InterpRes tyTRAITend
interpTLink c (IReq r) = ITraitL r c

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

interpProp : String
          -> List (InterpRes tyTRAIT)
          -> InterpRes tyPROPERTY
interpProp s ts = IProp pelem (Sigma.getProof elems)
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
    root = MkTask s Nothing

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
interpPatt s (IProb rP m) (ISolt rS is) = IPatt ((model \= root) \= (root &= [rP,rS]))
  where
    root : GLang ELEM
    root = MkGoal s Nothing

    model : GModel
    model = let (_ ** ds) = groupDecls is in
        DList.foldl (flip $ insert) m (trace (showDList show ds) ds)


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

getReqTitle : SifPriv i tyREQ -> String
getReqTitle (priv__mkReq ty t d) = t

-- -------------------------------------------------------------------- [ Show ]

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
toXML : SifPriv i ty -> Eff (Document ELEMENT) XEffs
toXML (priv__mkReq ty t d) = do
       let e  = addScore $ mkNDNode (cast ty) t d
       (idGen,_) <- get
       let idVal = cast {to=Int} (S idGen)
       let e' = setAttribute "id" (cast idVal) e
       update (\(idGen,ids) => ((S idGen), insert t (S idGen) ids))
       pure $ e'

toXML (priv__mkProb t d rs) = do
       let e = addScore $ mkNDNode "problem" t d
       -- The following mess is because I could get Effectful HOFs for DList to work.
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "requirements") rs''
       -- </mess>
       pure $ e <++> rNodes

toXML (priv__mkTLink cval r) = do
       (_,ids) <- get
       let id = lookup (getReqTitle r) ids
       let e = mkSimpleElement "affect"
       let idval = cast {to=Int} $ fromMaybe 0 id
       pure $ addScore $ (setAttribute "cvalue" (cast cval)
                 (setAttribute "linksTo" (cast idval) e))

toXML (priv__mkTrait ty t d s rs) = do
       let e  = addScore $ mkNDNode "trait" t d
       let e' = setAttribute "svalue" (cast s) e
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "affects") rs''
       -- </mess>
       pure $ e' <++> (addScore rNodes)

toXML (priv__mkProp t d rs) = do
       let e = addScore $ mkNDNode "property" t d
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "traits") rs''
       -- </mess>
       pure $ e <++> (addScore rNodes)

toXML (priv__mkSolt t d rs) = do
       let e = addScore $ mkNDNode "solution" t d
       -- <mess>
       let rs' = toLDP rs
       rs'' <- mapE (\r => toXML (Sigma.getProof r)) rs'
       let rNodes = foldl (\n,r => n <++> r) (mkSimpleElement "properties") rs''
       -- </mess>
       pure $ e
         <++> (addScore rNodes)
         <++> mkStructure
         <++> mkDynamics

toXML (priv__mkPatt t d p s) = do
       let e = addScore $ mkNDNode "pattern" t d
       pNode <- toXML p
       sNode <- toXML s
       pure $ e <++> mkRels
                <++> mkStudies
                <++> (addScore $ mkEmptyNode "evidence")
                <++> sNode
                <++> pNode
                <++> (addScore $ mkEmptyNode "context")
                <++> mkMdata

toXML _ = pure $ mkSimpleElement "bug"

-- --------------------------------------------------------------------- [ EOF ]
