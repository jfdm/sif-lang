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
    splitTimerMsg (getPFName !getBuildState) "Building Requirements"
    let p = mkProblem bob t d rs'
    updateBuildState (\st => record { getProb   = Just i
                                    , pattTitle = t} st)
    pure p

problemFromFile : SifBuilder impl
               -> String
               -> Eff (PROBLEM impl) SifEffs
problemFromFile bob f = do
    trace "Fetching problem specification"
    mkTimer f
    startTimer f
    past <- readSifFile problem f
    splitTimerMsg f "Finished Parsing now building"
    rval <- buildProblemE bob past
    stopTimer f
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
  splitTimerMsg (getSFName !getBuildState) "Building Affects"
  pure $ mkTrait bob ty t d v as'

buildPropertyE : SifBuilder impl
              -> SifAST tyPROPERTY
              -> Eff (PROPERTY impl) SifEffs
buildPropertyE bob (Property t d ts) = do
  debug $ unwords ["Building Property", show t]
  ts' <- mapE (\x => buildTraitE bob x) ts
  splitTimerMsg (getSFName !getBuildState) "Building Traits"
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
          splitTimerMsg (getSFName !getBuildState) "Building Properties"
          updateBuildState (\st => record
              { pattTitle = unwords [pattTitle st, "through",t]
              , pattDesc = pDesc
              } st)
          pure $ mkSolution bob t d ps'

solutionFromFile : SifBuilder impl
                -> String
                -> Eff (SOLUTION impl) SifEffs
solutionFromFile bob f = do
    trace "Fetching Solution Specification"
    mkTimer f
    startTimer f
    sast <- readSifFile solution f
    splitTimerMsg f "Finished Parsing now building"
    sval <- buildSolutionE bob sast
    stopTimer f
    pure sval

-- ----------------------------------------------------------------- [ Pattern ]

buildPatternE : SifBuilder impl
             -> PROBLEM impl
             -> SOLUTION impl
             -> Eff (PATTERN impl) SifEffs
buildPatternE bob p s = do
    st <- getBuildState
    trace $ unwords ["Building Pattern", show $ pattTitle st]
    let res = mkPattern bob (pattTitle st) (pattDesc st) p s
    splitTimerMsg (unwords [ "Building for "
                           , getPFName st
                           , "&"
                           , getSFName st]) "Finished Pattern"
    pure res

public
patternFromFile : SifBuilder impl
               -> String
               -> String
               -> Eff (PATTERN impl) SifEffs
patternFromFile bob p s = do
  putBuildState (defBuildSt p s)
  p' <- problemFromFile bob p
  splitTimerMsg (unwords ["Building for ", p, "&", s])
                "Finished Problem now Solution"
  s' <- solutionFromFile bob s
  splitTimerMsg (unwords ["Building for ", p, "&", s])
                "Finished Solution now Pattern"
  buildPatternE bob p' s'

-- --------------------------------------------------------------------- [ EOF ]
