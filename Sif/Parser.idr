-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser

import Lightyear
import Lightyear.Strings

import Sif.Effs

import public Sif.Parser.Problem
import public Sif.Parser.Pattern

import Sif.Pattern
import Sif.Error

import Sif.Parser.State

%default partial
%access private

public
readSifFile : Parser a
           -> String
           -> Eff a SifEffs
readSifFile p f = do
    trace $ unwords ["Reading file:", f]
    case !(open f Read) of
      True => do
        src <- readAcc ""
        close
        trace "Parsing File"
        case parse p src of
          Left err  => Sif.raise (ParseError f err)
          Right res => pure res
      False => Sif.raise (FileMissing f)
  where
    readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
    readAcc acc = if (not !(eof))
                     then readAcc (acc ++ !(readLine))
                     else pure acc

-- ----------------------------------------------------------- [ Build Problem ]
conRQ : RTy -> String -> Maybe String -> REQUIREMENT
conRQ FUNC s d = mkFunctional s d
conRQ USAB s d = mkUsability s d
conRQ RELI s d = mkReliability s d
conRQ PERF s d = mkPerformance s d
conRQ SUPP s d = mkSupportability s d

doRQ : ProbAST ReqTy -> Eff REQUIREMENT SifEffs
doRQ (MkReq i ty t d) = do
  st <- getBuildState
  case lookup i (getRQs st) of
    Just x  => Sif.raise (DuplicateID i)
    Nothing => do
      let r = conRQ ty t d
      putBuildState (record { getRQs = (i,r) :: (getRQs st)} st)
      pure r

doRQs : List (ProbAST ReqTy) -> Eff (List REQUIREMENT) SifEffs
doRQs xs = do
  rval <- mapE (\x => doRQ x) xs
  pure rval


getProblemEff : ProbAST ProbTy -> Eff PROBLEM SifEffs
getProblemEff (MkProb i t d rs) = do
    trace "Building problem specification"
    rs' <- doRQs rs
    let p = mkProblem t d rs'
    updateBuildState (\st => record { getProb   = (i, Just p)
                                    , pattTitle = t} st)
    pure p

problemFromFile : String -> Eff PROBLEM SifEffs
problemFromFile s = do
    trace "Fetching problem specification"
    past <- readSifFile problem s
    rval <- getProblemEff past
    pure $ rval

-- ---------------------------------------------------------- [ Build Solution ]


buildAffect : SolAST AffectTy -> Eff TLINK SifEffs
buildAffect (Affects c i d) = do
  debug $ unwords ["Building Affect for", show i]
  st <- getBuildState
  case lookup i (getRQs st) of
    Nothing => Sif.raise (IDMissing i)
    Just r  => pure $ mkLink c r d

buildTrait : SolAST TraitTy -> Eff TRAIT SifEffs
buildTrait (Trait ty t v d as) = do
  debug $ unwords ["Building Trait", show t]
  as' <- mapE (\x => buildAffect x) as
  case ty of
    GEN => pure $ mkTrait t d v as'
    ADV => pure $ mkAdvantage t d v as'
    DIS => pure $ mkDisadvantage t d v as'

buildProperty : SolAST PropTy -> Eff PROPERTY SifEffs
buildProperty (Property t d ts) = do
  debug $ unwords ["Building Property", show t]
  ts' <- mapE (\x => buildTrait x) ts
  pure $ mkProperty t d ts'

buildSolution : SolAST SolTy -> Eff SOLUTION SifEffs
buildSolution (Solution t pID d ps) = do
  trace "Building solution specification"
  st <- getBuildState
  let pID' = fst $ getProb st
  if not (pID' == pID)
    then Sif.raise (ProblemMissing pID')
    else do
      ps' <- mapE (\p => buildProperty p) ps
      updateBuildState (\st => record {pattTitle = unwords [pattTitle st, "through",t]} st)
      pure $ mkSolution t d ps'

solutionFromFile : String -> Eff SOLUTION SifEffs
solutionFromFile f = do
    trace "Fetching Solution Specification"
    (pd, sast) <- readSifFile solution f
    updateBuildState (\st => record {pattDesc = pd} st)
    sval <- buildSolution sast
    pure sval

-- ----------------------------------------------------------- [ Build Pattern ]

public
buildPatternFromFile : String
                    -> String
                    -> Eff PATTERN SifEffs
buildPatternFromFile p s = do
  putBuildState (defBuildSt)
  p' <- problemFromFile p
  s' <- solutionFromFile s
  st <- getBuildState
  trace $ unwords ["Building Pattern", show $ pattTitle st]
  pure $ mkPattern (pattTitle st) (pattDesc st) p' s'


-- --------------------------------------------------------------------- [ EOF ]
