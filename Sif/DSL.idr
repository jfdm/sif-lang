-- ----------------------------------------------------------------- [ DSL.idr ]
-- Module    : DSL.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Build a Sif Model from the AST.
module Sif.DSL

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
import Lightyear.Strings

import Sif.Types
import Sif.AbsSyntax
import Sif.Error
import Sif.Effs
import Sif.Pattern

import Sif.DSL.State
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution
import Sif.DSL.Parser

-- -------------------------------------------------------------- [ Directives ]
%access private
%default partial

-- ----------------------------------------------------------- [ Build Problem ]

buildReqE : SifBuilder impl
         -> SifAST tyREQ
         -> Eff (REQUIREMENT impl) SifEffs
buildReqE bob q@(Req i ty t d) = do
  st <- getBuildState
  case lookup i (getRQs st) of
    Just x  => Sif.raise (DuplicateID i)
    Nothing => do
      let r = mkRequirement bob ty t d
      putBuildState (record { getRQs = (i,q) :: (getRQs st)} st)
      pure r

buildProblemE : SifBuilder impl
             -> SifAST tyPROBLEM
             -> Eff (PROBLEM impl) SifEffs
buildProblemE bob (Problem i t d rs) = do
    trace "Building problem specification"
    rs' <- mapE (\r => buildReqE bob r) rs
    let p = mkProblem bob t d rs'
    updateBuildState (\st => record { getProb   = Just i
                                    , pattTitle = t} st)
    pure p

problemFromFile : SifBuilder impl
               -> String
               -> Eff (PROBLEM impl) SifEffs
problemFromFile bob s = do
    trace "Fetching problem specification"
    past <- readSifFile problem s
    rval <- buildProblemE bob past
    pure $ rval

-- ---------------------------------------------------------- [ Build Solution ]


buildAffectE : SifBuilder impl
            -> SifAST tyAFFECTS
            -> Eff (AFFECT impl) SifEffs
buildAffectE bob (Affect c i d) = do
  debug $ unwords ["Building Affect for", show i]
  st <- getBuildState
  case lookup i (getRQs st) of
    Nothing => Sif.raise (IDMissing i)
    Just (Req i' ty t d')  => do
      let r = mkRequirement bob ty t d'
      pure $ mkAffect bob c r d

buildTraitE : SifBuilder impl
           -> SifAST tyTRAIT
           -> Eff (TRAIT impl) SifEffs
buildTraitE bob (Trait ty t v d as) = do
  debug $ unwords ["Building Trait", show t]
  as' <- mapE (\x => buildAffectE bob x) as
  pure $ mkTrait bob ty t d v as'

buildPropertyE : SifBuilder impl
              -> SifAST tyPROPERTY
              -> Eff (PROPERTY impl) SifEffs
buildPropertyE bob (Property t d ts) = do
  debug $ unwords ["Building Property", show t]
  ts' <- mapE (\x => buildTraitE bob x) ts
  pure $ mkProperty bob t d ts'

buildSolutionE : SifBuilder impl
              -> SifAST tySOLUTION
              -> Eff (SOLUTION impl) SifEffs
buildSolutionE bob (Solution t (pID,pDesc) d ps) = do
  trace "Building solution specification"
  st <- getBuildState

  case getProb st of
    Nothing => Sif.raise (ProblemMissing "")
    Just pID' => do
      if not (pID' == pID)
        then Sif.raise (ProblemMissing pID')
        else do
          ps' <- mapE (\p => buildPropertyE bob p) ps
          updateBuildState (\st => record {pattTitle = unwords [pattTitle st, "through",t]} st)
          updateBuildState (\st => record {pattDesc = pDesc} st)
          pure $ mkSolution bob t d ps'

solutionFromFile : SifBuilder impl
                -> String
                -> Eff (SOLUTION impl) SifEffs
solutionFromFile bob f = do
    trace "Fetching Solution Specification"
    sast <- readSifFile solution f
    sval <- buildSolutionE bob sast
    pure sval

-- ----------------------------------------------------------------- [ Pattern ]

public
patternFromFile : SifBuilder impl
               -> String
               -> String
               -> Eff (PATTERN impl) SifEffs
patternFromFile bob p s = do
  putBuildState (defBuildSt)
  p' <- problemFromFile bob p
  s' <- solutionFromFile bob s
  st <- getBuildState
  trace $ unwords ["Building Pattern", show $ pattTitle st]
  pure $ mkPattern bob (pattTitle st) (pattDesc st) p' s'

-- --------------------------------------------------------------------- [ EOF ]
