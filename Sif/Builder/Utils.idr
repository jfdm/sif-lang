-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Builder.Utils

import GRL.Lang.GLang

import Sif.Types
import Sif.Pattern

-- -------------------------------------------------------------- [ Directives ]

%access public
%default partial

-- --------------------------------------------------- [ Interpretation Result ]

instance SifMetaModel GModel where
  toString x = GLang.toString x

data InterpRes : SifTy -> Type where
  IReq    : GLang ELEM -> InterpRes TyREQ
  IProb   : GLang ELEM -> GModel               -> InterpRes TyPROBLEM
  IAffect : GLang ELEM -> CValue               -> InterpRes TyAFFECTS
  ITrait  : GLang ELEM -> List (GLang INTENT)  -> InterpRes TyTRAIT
  IProp   : GLang ELEM -> DList GTy GLang es   -> InterpRes TyPROPERTY
  ISolt   : GLang ELEM -> DList GTy GLang ss   -> InterpRes TySOLUTION
  IPatt   : GModel                             -> InterpRes TyPATTERN

interpReq : String -> InterpRes TyREQ
interpReq s = IReq root
  where
    root : GLang ELEM
    root = mkGoal ("Requirement: " ++ s)

interpProb : String -> List (InterpRes TyREQ) -> InterpRes TyPROBLEM
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

interpAffect : CValue -> InterpRes TyREQ -> InterpRes TyAFFECTS
interpAffect c (IReq r) = IAffect r c

interpTrait : String
           -> SValue
           -> List (InterpRes TyAFFECTS)
           -> TTy
           -> InterpRes TyTRAIT
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
    cs = map (\(IAffect r c) => node ==> r | c) es

interpProp : String
          -> List (InterpRes TyTRAIT)
          -> InterpRes TyPROPERTY
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
          -> List (InterpRes TyPROPERTY)
          -> InterpRes TySOLUTION
interpSolt s ps = ISolt root (Sigma.getProof elems)
  where
    root : GLang ELEM
    root = mkTask ("Solution: " ++ s)

    cs : GLang STRUCT
    cs = (root &= map (\(IProp x ys) => x) ps)

    doGet : InterpRes TyPROPERTY
         -> (is ** DList GTy GLang is)
         -> (xs ** DList GTy GLang xs)
    doGet (IProp x ys) (is ** res) = (_ ** ys ++ res)

    getDecls : (as ** DList GTy GLang as)
    getDecls = foldr (\e,res => doGet e res) (_ ** DList.Nil)  ps

    elems : (es ** DList GTy GLang es)
    elems = (_ ** [root, cs] ++ (Sigma.getProof getDecls))

interpPatt : InterpRes TyPROBLEM
          -> InterpRes TySOLUTION
          -> InterpRes TyPATTERN
interpPatt (IProb rP m) (ISolt rS is) = IPatt (insertDecls is m)


-- --------------------------------------------------------------------- [ EOF ]
