-- ----------------------------------------------------------------- [ DSL.idr ]
-- Module    : DSL.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Build a Sif Model from the AST.
module Sif.DSL

-- ----------------------------------------------------------------- [ Imports ]
import Effects
import Effect.State
import Effect.Default
import Effect.Perf
import Effect.File
import Effect.Exception
import Effect.Logging.Default

import Lightyear
import Lightyear.Strings

import Sif.Types
import Sif.AbsSyntax
import Sif.Error

import Sif.Pattern

import Sif.DSL.State
import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution
import Sif.DSL.Parser

-- -------------------------------------------------------------- [ Directives ]
%access private
%default partial

-- @TODO de exception this...

BuildEffs : List EFFECT
BuildEffs = [ FILE_IO ()
            , LOG
            , PERF
            , 'sif    ::: EXCEPTION SifError
            , 'bstate ::: STATE BuildState
            ]

-- ----------------------------------------------------------- [ Build Helpers ]

getBuildState : Eff BuildState ['bstate ::: STATE BuildState]
getBuildState = pure $ !('bstate :- get)

putBuildState : BuildState -> Eff () ['bstate ::: STATE BuildState]
putBuildState st = 'bstate :- put st

updateBuildState : (BuildState -> BuildState) -> Eff () ['bstate ::: STATE BuildState]
updateBuildState f = 'bstate :- update (\st => f st)

DSLBuilder : Type -> Type
DSLBuilder rTy = Eff rTy BuildEffs

-- ----------------------------------------------------------- [ Build Problem ]

buildReqE : SifBuilder impl
         -> (d : SifDomain)
         -> SifAST TyREQ
         -> DSLBuilder $ REQUIREMENT impl d
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
             -> DSLBuilder (d ** PROBLEM impl d)
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
               -> DSLBuilder (d ** PROBLEM impl d)
problemFromFile bob f = do
    trace "Fetching problem specification"
    mkTimer f
    startTimer f
    (Right past) <- readSifFile problem f | Left err => Sif.raise err
    splitTimerMsg f "Finished Parsing now building"
    rval <- buildProblemE bob past
    stopTimer f
    pure $ rval

-- ---------------------------------------------------------- [ Build Solution ]

buildAffectE : SifBuilder impl
            -> (d : SifDomain)
            -> SifAST TyAFFECTS
            -> DSLBuilder (AFFECT impl d)
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
           -> DSLBuilder (TRAIT impl d)
buildTraitE bob c (Trait ty t v d as) = do
  debug $ unwords ["Building Trait", show t]
  as' <- mapE (\x => buildAffectE bob c x) as
  splitTimerMsg (getSFName !getBuildState) "Building Affects"
  pure $ mkTrait bob c ty t d v as'

buildPropertyE : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TyPROPERTY
              -> DSLBuilder (PROPERTY impl d)
buildPropertyE bob c (Property t d ts) = do
  debug $ unwords ["Building Property", show t]
  ts' <- mapE (\x => buildTraitE bob c x) ts
  splitTimerMsg (getSFName !getBuildState) "Building Traits"
  pure $ mkProperty bob c t d ts'

buildSolutionE : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TySOLUTION
              -> DSLBuilder (SOLUTION impl d)
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
                -> DSLBuilder (SOLUTION impl d)
solutionFromFile bob c f = do
    trace "Fetching Solution Specification"
    mkTimer f
    startTimer f
    (Right sast) <- readSifFile solution f | Left err => Sif.raise err
    splitTimerMsg f "Finished Parsing now building"
    sval <- buildSolutionE bob c sast
    stopTimer f
    pure sval

-- ----------------------------------------------------------------- [ Pattern ]

buildPatternE : SifBuilder impl
             -> (d : SifDomain)
             -> PROBLEM impl d
             -> SOLUTION impl d
             -> DSLBuilder (PATTERN impl d)
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
               -> DSLBuilder (d ** PATTERN impl d)
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
