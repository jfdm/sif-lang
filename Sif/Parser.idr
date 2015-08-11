-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser

import public Lightyear
import public Lightyear.Strings

import Sif.Effs

import public Sif.Parser.Problem
import public Sif.Parser.Pattern

import Sif.Pattern
import public Sif.Parser.File
import public Sif.Parser.State

%default partial
%access private

public
BuildEffs : List EFFECT
BuildEffs = [ FILE_IO ()
            , 'sif ::: EXCEPTION SifError
            , 'bst ::: STATE BuildEnv]

-- ----------------------------------------------------------- [ Build Problem ]
conRQ : RTy -> String -> Maybe String -> REQUIREMENT
conRQ FUNC s d = mkFunctional s d
conRQ USAB s d = mkUsability s d
conRQ RELI s d = mkReliability s d
conRQ PERF s d = mkPerformance s d
conRQ SUPP s d = mkSupportability s d

doRQ : ProbAST ReqTy -> Eff REQUIREMENT BuildEffs
doRQ (MkReq i ty t d) = do
  st <- 'bst :- get
  case lookup i (getRQs st) of
    Just x  => Sif.raise (DuplicateID i)
    Nothing => do
      let r = conRQ ty t d
      'bst :- put (record { getRQs = (i,r) :: (getRQs st)} st)
      pure r

doRQs : List (ProbAST ReqTy) -> Eff (List REQUIREMENT) BuildEffs
doRQs xs = do
  rval <- mapE (\x => doRQ x) xs
  pure rval


getProblemEff : ProbAST ProbTy -> Eff PROBLEM BuildEffs
getProblemEff (MkProb i t d rs) = do
    rs' <- doRQs rs
    let p = mkProblem t d rs'
    'bst :- update (\st => record {getProb = (i, Just p)} st)
    pure p

problemFromFile : String -> Eff PROBLEM BuildEffs
problemFromFile s = do
    past <- readSifFile problem s
    rval <- getProblemEff past
    pure $ rval

-- ---------------------------------------------------------- [ Build Solution ]


buildAffect : SolAST AffectTy -> Eff TLINK BuildEffs
buildAffect (Affects c i) = do
  st <- 'bst :- get
  case lookup i (getRQs st) of
    Nothing => Sif.raise (IDMissing i)
    Just r  => pure $ mkLink c r

buildTrait : SolAST TraitTy -> Eff TRAIT BuildEffs
buildTrait (Trait ty t v d as) = do
  as' <- mapE (\x => buildAffect x) as
  case ty of
    ADV => pure $ mkAdvantage t d v as'
    DIS => pure $ mkDisadvantage t d v as'

buildProperty : SolAST PropTy -> Eff PROPERTY BuildEffs
buildProperty (Property t d ts) = do
  ts' <- mapE (\x => buildTrait x) ts
  pure $ mkProperty t d ts'

buildSolution : SolAST SolTy -> Eff SOLUTION BuildEffs
buildSolution (Solution t pID d ps) = do
  st <- 'bst :- get
  let pID' = fst $ getProb st
  if not (pID' == pID)
    then Sif.raise (ProblemMissing pID')
    else do
      ps' <- mapE (\p => buildProperty p) ps
      pure $ mkSolution t d ps'

solutionFromFile : String -> Eff SOLUTION BuildEffs
solutionFromFile f = do
    (pt, sast) <- readSifFile solution f
    'bst :- update (\st => record {getPData = pt} st)
    sval <- buildSolution sast
    pure sval

-- ----------------------------------------------------------- [ Build Pattern ]

public
buildPatternFromFile : String
                    -> String
                    -> Eff PATTERN BuildEffs
buildPatternFromFile p s = do
  'bst :- put (defBuildSt)
  p' <- problemFromFile p
  s' <- solutionFromFile s
  st <- 'bst :- get
  pure $ mkPattern (fst $ getPData st) (snd $ getPData st) p' s'


-- --------------------------------------------------------------------- [ EOF ]
