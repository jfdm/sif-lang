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

%default partial

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
    sVal v = case ty of  -- Disadvantages...
               GEN => v
               ADV => v
               DIS => invertEval v

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
               -> (req : SifPriv r tyREQ)
               -> (desc : Maybe String)
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
              -> SifPriv (interpPatt p s) tyPATTERN

covering
getPrivTitle : SifPriv i ty -> Maybe String
getPrivTitle (priv__mkReq _ t _)       = Just t
getPrivTitle (priv__mkProb t _ _)      = Just t
getPrivTitle (priv__mkTrait _ t _ _ _) = Just t
getPrivTitle (priv__mkProp t _ _)      = Just t
getPrivTitle (priv__mkSolt t _ _)      = Just t
getPrivTitle (priv__mkPatt t _ _ _)    = Just t
getPrivTitle _                         = Nothing

getReqTitle : SifPriv i tyREQ -> String
getReqTitle (priv__mkReq ty t d) = t
getReqTitle _                    = ""

-- -------------------------------------------------------------------- [ Show ]

covering
showSifPriv : SifPriv i ty -> String
showSifPriv (priv__mkReq ty t d) = with List
    unwords ["   ", show ty, show t]

showSifPriv (priv__mkProb t d rs) = with List
    unwords [ "  Problem:", show t, "\n"
            , unlines $ mapDList (\x => showSifPriv x) rs
            , "\n"]

showSifPriv (priv__mkTLink cval r d) = with List
    unwords ["       ", show cval, (getReqTitle r), "\n"]

showSifPriv (priv__mkTrait ty t d s rs) = with List
    unwords [
        "     ", show ty, ":" , show t , "is", show s , "\n"
      , concat (mapDList (showSifPriv) rs)
      , "\n"]

showSifPriv (priv__mkProp t d ts) = with List
    unwords [
        "     Property: " , t , "\n"
     , concat (mapDList (showSifPriv) ts)
     , "\n"]

showSifPriv (priv__mkSolt t d ps) = with List
    unwords [
         "  Solution: " , t , "\n"
      , Foldable.concat (mapDList showSifPriv ps)
      , "\n"]

showSifPriv (priv__mkPatt t d p s) = with List
    unwords [
         "Pattern:" , show t , "\n"
      , showSifPriv p
      , showSifPriv s]

-- ------------------------------------------------------------------- [ toORG ]

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
toXML : SifPriv i ty -> Document DOCUMENT
toXML p = setRoot root $ mkDocument (mkQName "pattern") Nothing
  where
    partial
    root : Document ELEMENT
    root = runPureInit [(Z,Dict.empty)] (toXML' p)



-- -------------------------------------------------------------------- [ Edda ]

toEdda : SifPriv i tyPATTERN -> Maybe $ Edda PRIME MODEL
toEdda expr =
    case parse parseOrg (toOrg expr) of
      Left  err => Nothing
      Right doc => Just (refineEdda doc)

-- ----------------------------------------------------- [ Instances and Stuff ]


getModel : {x : InterpRes tyPATTERN} -> SifPriv x tyPATTERN -> GModel
getModel {x} _ = extract x
  where
    extract : InterpRes tyPATTERN -> GModel
    extract (IPatt m) = m
    extract _         = emptyModel

-- --------------------------------------------------------------------- [ DOT ]

toDot : {i : InterpRes tyPATTERN} -> SifPriv i tyPATTERN -> SimpleDot GRAPH
toDot p = grlToDot $ getModel p

covering partial
convPriv : {i : InterpRes tyPATTERN}
        -> SifPriv i tyPATTERN
        -> (fmt : SifOutFormat)
        -> Maybe (convTy fmt)
convPriv p ORG     = Just $ toOrg p
convPriv p XML     = Just $ toXML p
convPriv p DOT     = Just $ toDot p
convPriv p GRL     = Just $ getModel p
convPriv p EDDA    = toEdda p
convPriv p COMPACT = Just $ showSifPriv p
convPriv p IDRIS   = Nothing


data SifWrapper : SifTy -> Type where
  MkWrapper : {i : InterpRes ty} -> SifPriv i ty -> SifWrapper ty

instance Show (SifPriv i ty) where
  show = showSifPriv

instance SifRepAPI SifWrapper where
  getTitle (MkWrapper i) = getPrivTitle i


  evalPattern (MkWrapper p) =
      case evalModel (getModel p) Nothing of
        BadModel  => Bad
        Result gs => Good $ map (\x => (getNodeTitle x, getSValue x)) gs

  toString (MkWrapper p)      = showSifPriv p
  convTo (MkWrapper   x) fmt  = convPriv x fmt

-- ------------------------------------------------------------- [ The Builder ]

buildReqAbs : RTy -> String -> Maybe String -> REQUIREMENT SifWrapper
buildReqAbs ty s d = MkExpr $ MkWrapper $ priv__mkReq ty s d

conv : List (SifExpr SifWrapper ty)
    -> (xs ** DList (InterpRes ty) (\x => SifPriv x ty) xs)
conv xs = fromLDP $ map (\(MkExpr (MkWrapper x)) => (_ ** x)) xs

convR : SifExpr SifWrapper tyREQ -> (r : InterpRes tyREQ ** SifPriv r tyREQ)
convR (MkExpr (MkWrapper res)) = (_ ** res)


buildProblemAbs : String -> Maybe String -> REQUIREMENTS SifWrapper -> PROBLEM SifWrapper
buildProblemAbs t d rs =
    MkExpr $ MkWrapper $ priv__mkProb t d (Sigma.getProof $ conv rs)

buildAffectAbs : CValue -> REQUIREMENT SifWrapper -> Maybe String -> AFFECT SifWrapper
buildAffectAbs c r d = MkExpr $ MkWrapper $ priv__mkTLink c (Sigma.getProof $ convR r) d

buildTraitAbs : TTy
             -> String
             -> Maybe String
             -> SValue
             -> AFFECTS SifWrapper
             -> TRAIT SifWrapper
buildTraitAbs ty t d s rs =
    MkExpr $ MkWrapper $ priv__mkTrait ty t d s (Sigma.getProof $ conv rs)


buildPropertyAbs : String -> Maybe String -> TRAITS SifWrapper -> PROPERTY SifWrapper
buildPropertyAbs t d ts =
    MkExpr $ MkWrapper $ priv__mkProp t d (Sigma.getProof $ conv ts)

buildSolutionAbs : String -> Maybe String -> PROPERTIES SifWrapper -> SOLUTION SifWrapper
buildSolutionAbs s d ps =
    MkExpr $ MkWrapper $ priv__mkSolt s d (Sigma.getProof $ conv ps)


{-

  buildPattern  : String -> Maybe String -> PROBLEM -> SOLUTION -> PATTERN

-}

buildPatternAbs  : String -> Maybe String -> PROBLEM SifWrapper -> SOLUTION SifWrapper -> PATTERN SifWrapper
buildPatternAbs t d (MkExpr (MkWrapper p)) (MkExpr (MkWrapper s)) = MkExpr $ MkWrapper $ priv__mkPatt t d p s


defBuilder : SifBuilder SifWrapper
defBuilder = MkSifBuilder
    buildReqAbs
    buildProblemAbs
    buildAffectAbs
    buildTraitAbs
    buildPropertyAbs
    buildSolutionAbs
    buildPatternAbs

-- --------------------------------------------------------------------- [ EOF ]
