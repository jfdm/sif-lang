-- ---------------------------------------------------------------- [ Edda.idr ]
-- Module    : Edda.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Convert.Edda

import Lightyear
import Lightyear.Strings

import GRL.Lang.GLang

import Edda
import Edda.Reader.Org

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API

-- import Sif.Pattern.Convert.Org

-- -------------------------------------------------------------- [ Directives ]

%access private
%default partial


-- ------------------------------------------------------------------- [ Utils ]

para' : String -> Edda STAR BLOCK
para' s =
  case parse para s of
    Left err  => Empty STAR
    Right res => res

inline' : String -> (List $ Edda STAR INLINE)
inline' s =
  case parse (some inline) s of
    Left err  => [Font SerifTy "Error"]
    Right res => res

sectionTemplate : Nat -> Maybe String -> String -> Edda STAR BLOCK
sectionTemplate n l t = Section STAR n l (inline' t) Nil

section' : Maybe String -> String -> Edda STAR BLOCK
section' l t = sectionTemplate 0 l t

subsection' : Maybe String -> String -> Edda STAR BLOCK
subsection' l t = sectionTemplate 1 l t

subsubsection' : Maybe String -> String -> Edda STAR BLOCK
subsubsection' l t = sectionTemplate 2 l t

subsubsubsection' : Maybe String -> String -> Edda STAR BLOCK
subsubsubsection' l t = sectionTemplate 3 l t

mkDescPara : Maybe String -> Edda STAR BLOCK
mkDescPara Nothing  = para' "Descriptino Missing\n\n"
mkDescPara (Just d) = para' (unlines [d,"\n"])

-- ------------------------------------------------------- [ Convert Functions ]

-- ---------------------------------------------------------------- [ Problems ]
convertReq : REQUIREMENT impl d -> List $ Edda STAR BLOCK
convertReq r = with SifExpr
    [ subsection' Nothing
        (unwords [show (getRTy r), ":", getTitle r])
    , mkDescPara (getDesc r)]

convertProblem : PROBLEM impl d -> List $ Edda STAR BLOCK
convertProblem p = with SifExpr
    [ section' Nothing (unwords ["Problem:", getTitle p])
    , mkDescPara (getDesc p)
    , section' Nothing "Forces"]
    ++ convRS
  where
    convRS : List $ Edda STAR BLOCK
    convRS = concatMap (convertReq) $ SifExpr.getReqs p

-- ---------------------------------------------------------------- [ Solution ]

convertAffect : AFFECT impl d -> List $ Edda STAR BLOCK
convertAffect a = [t,d]
  where
    t : Edda STAR BLOCK
    t = subsubsubsection' Nothing
           (unwords [ "/", show (SifExpr.getCValue a), "/"
                    , SifExpr.getTitle $ SifExpr.getReq a])

    d : Edda STAR BLOCK
    d = mkDescPara (SifExpr.getDesc a)

convertTrait : TRAIT impl d -> List $ Edda STAR BLOCK
convertTrait t = [tit,sVal,d] ++ as
  where
    tit : Edda STAR BLOCK
    tit = subsubsection' Nothing
            (unwords [ "Trait", show (SifExpr.getTTy t), ":"
                     , SifExpr.getTitle t])

    sVal : Edda STAR BLOCK
    sVal = DList STAR [MkPair (inline' "Evaluation Value")
                              (inline' $ show (SifExpr.getSValue t))]

    d : Edda STAR BLOCK
    d = mkDescPara (SifExpr.getDesc t)

    as : List $ Edda STAR BLOCK
    as = concatMap (convertAffect) $ SifExpr.getAffects t

convertProperty : PROPERTY impl d -> List $ Edda STAR BLOCK
convertProperty p = [t,d] ++ ts
  where
    t : Edda STAR BLOCK
    t = subsection' Nothing
          (unwords [ "Property:", SifExpr.getTitle p])

    d : Edda STAR BLOCK
    d = mkDescPara (SifExpr.getDesc p)

    ts : List $ Edda STAR BLOCK
    ts = concatMap (convertTrait) $ SifExpr.getTraits p

convertSolution : SOLUTION impl d -> List $ Edda STAR BLOCK
convertSolution s = [t,d] ++ ps
  where
    t : Edda STAR BLOCK
    t = section' Nothing
            (unwords [ "Solution:", SifExpr.getTitle s])

    d : Edda STAR BLOCK
    d = mkDescPara (SifExpr.getDesc s)

    ps : List $ Edda STAR BLOCK
    ps = concatMap (convertProperty) $ SifExpr.getProperties s

convertDomain : SifDomain -> List $ Edda STAR BLOCK
convertDomain c = [t,d]
  where
    t : Edda STAR BLOCK
    t = section' Nothing
          (unwords ["Context:", getTitle c])
    d : Edda STAR BLOCK
    d = mkDescPara (getDesc c)

convertPattern : PATTERN impl d -> Edda STAR MODEL
convertPattern p = EddaRaw as (intersperse (Empty STAR) body)
  where
    as : List (String, String)
    as = [MkPair "TITLE" (SifExpr.getTitle p)]

    dom : List $ Edda STAR BLOCK
    dom = convertDomain $ SifExpr.getDomain p

    prob : List $ Edda STAR BLOCK
    prob = convertProblem $ SifExpr.getProblem p

    sol : List $ Edda STAR BLOCK
    sol = convertSolution $ SifExpr.getSolution p

    body : List $ Edda STAR BLOCK
    body = [mkDescPara (SifExpr.getDesc p)] ++ dom ++ prob ++ sol


public
toEdda : PATTERN impl d -> Edda PRIME MODEL
toEdda pdoc = refineEdda (convertPattern pdoc)

-- --------------------------------------------------------------------- [ EOF ]
