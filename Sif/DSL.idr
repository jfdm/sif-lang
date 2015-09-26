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
         -> (d : SifDomain)
         -> SifAST TyREQ
         -> Eff (REQUIREMENT impl d) SifEffs
buildReqE bob c q@(Req i ty t d) = do
  st <- getBuildState
  case lookup i (getRQs st) of
    Just x  => Sif.raise (DuplicateID i)
    Nothing => do
      let r = mkRequirement bob c ty t d
      putBuildState (record { getRQs = (i,q) :: (getRQs st)} st)
      pure r

buildProblemE : SifBuilder impl
             -> SifAST TyPROBLEM
             -> Eff (d ** PROBLEM impl d) SifEffs
buildProblemE bob (Problem i t d (cID,c) rs) = do
    trace "Building problem specification"
    rs' <- mapE (\r => buildReqE bob c r) rs
    splitTimerMsg (getPFName !getBuildState) "Building Requirements"
    let p = mkProblem bob c t d rs'
    updateBuildState (\st => record { getProb     = Just i
                                    , pattTitle   = t
                                    , getDomainID = Just cID
                                    , getDomain   = c} st)
    pure (_ ** p)

problemFromFile : SifBuilder impl
               -> String
               -> Eff (d ** PROBLEM impl d) SifEffs
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
            -> (d : SifDomain)
            -> SifAST TyAFFECTS
            -> Eff (AFFECT impl d) SifEffs
buildAffectE bob ctxt (Affect c i d) = do
  debug $ unwords ["Building Affect for", show i]
  st <- getBuildState
  case lookup i (getRQs st) of
    Nothing => Sif.raise (IDMissing i)
    Just (Req i' ty t d')  => do
      let r = mkRequirement bob ctxt ty t d'
      pure $ mkAffect bob ctxt c r d

buildTraitE : SifBuilder impl
           -> (d : SifDomain)
           -> SifAST TyTRAIT
           -> Eff (TRAIT impl d) SifEffs
buildTraitE bob c (Trait ty t v d as) = do
  debug $ unwords ["Building Trait", show t]
  as' <- mapE (\x => buildAffectE bob c x) as
  splitTimerMsg (getSFName !getBuildState) "Building Affects"
  pure $ mkTrait bob c ty t d v as'

buildPropertyE : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TyPROPERTY
              -> Eff (PROPERTY impl d) SifEffs
buildPropertyE bob c (Property t d ts) = do
  debug $ unwords ["Building Property", show t]
  ts' <- mapE (\x => buildTraitE bob c x) ts
  splitTimerMsg (getSFName !getBuildState) "Building Traits"
  pure $ mkProperty bob c t d ts'

buildSolutionE : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TySOLUTION
              -> Eff (SOLUTION impl d) SifEffs
buildSolutionE bob c (Solution t (pID, pDesc) d cID ps) = do
  trace "Building solution specification"
  st <- getBuildState

  case (getProb st, getDomainID st ) of
    (Nothing, Nothing) => Sif.raise InternalErr
    (_,       Nothing) => Sif.raise InternalErr
    (Nothing, _)       => Sif.raise InternalErr
    (Just pID', Just cID') => do
      case (not (pID' == pID)) of
        True => Sif.raise (MismatchError pID pID')
        False =>
          case not (cID' == cID) of
            True  => Sif.raise (MismatchError cID cID')
            False => do
              ps' <- mapE (\p => buildPropertyE bob c p) ps
              splitTimerMsg (getSFName !getBuildState) "Building Properties"
              updateBuildState (\st => record
                  { pattTitle = unwords [pattTitle st, "through",t]
                  , pattDesc = pDesc
                  } st)
              pure $ mkSolution bob c t d ps'

solutionFromFile : SifBuilder impl
                -> (d : SifDomain)
                -> String
                -> Eff (SOLUTION impl d) SifEffs
solutionFromFile bob c f = do
    trace "Fetching Solution Specification"
    mkTimer f
    startTimer f
    sast <- readSifFile solution f
    splitTimerMsg f "Finished Parsing now building"
    sval <- buildSolutionE bob c sast
    stopTimer f
    pure sval

-- ----------------------------------------------------------------- [ Pattern ]

buildPatternE : SifBuilder impl
             -> (d : SifDomain)
             -> PROBLEM impl d
             -> SOLUTION impl d
             -> Eff (PATTERN impl d) SifEffs
buildPatternE bob c p s = do
    st <- getBuildState
    trace $ unwords ["Building Pattern", show $ pattTitle st]
    let res = mkPattern bob c (pattTitle st) (pattDesc st) p s
    splitTimerMsg (unwords [ "Building for "
                           , getPFName st
                           , "&"
                           , getSFName st]) "Finished Pattern"
    pure res

public
patternFromFile : SifBuilder impl
               -> String
               -> String
               -> Eff (d ** PATTERN impl d) SifEffs
patternFromFile bob p s = do
  putBuildState (defBuildSt p s)
  (c ** p') <- problemFromFile bob p
  splitTimerMsg (unwords ["Building for ", p, "&", s])
                "Finished Problem now Solution"
  s' <- solutionFromFile bob c s
  splitTimerMsg (unwords ["Building for ", p, "&", s])
                "Finished Solution now Pattern"
  res <- buildPatternE bob c p' s'
  pure (c ** res)

-- --------------------------------------------------------------------- [ EOF ]
