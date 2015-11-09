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
             -> (d : SifDomain)
             -> SifAST TyPROBLEM
             -> DSLBuilder (PROBLEM impl d)
buildProblemE bob d (Problem i t d' _ rs) = do
    trace "Building problem specification"
    rs' <- mapE (\r => buildReqE bob d r) rs
    splitTimerMsg (getPFName !getBuildState) "Building Requirements"
    let p = mkProblem bob d t d' rs'
    updateBuildState (\st => record { getProbID     = Just i
                                    , pattTitle   = t} st)
    pure p

problemFromAST : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TyPROBLEM
              -> DSLBuilder (PROBLEM impl d)
problemFromAST bob d past = do
    mkTimer (getPFName !getBuildState)
    startTimer (getPFName !getBuildState)
    rval <- buildProblemE bob d past
    stopTimer (getPFName !getBuildState)
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
buildSolutionE bob d (Solution t (pID, pDesc) d' _ ps) = do
  trace "Building solution specification"
  st <- getBuildState
  trace "Comparing "
  case getProbID st of
    Nothing   => Sif.raise InternalErr
    Just pID' => do
      case (not (pID' == pID)) of
        True => Sif.raise (MismatchError pID pID')
        False => do
          ps' <- mapE (\p => buildPropertyE bob d p) ps
          splitTimerMsg (getSFName !getBuildState) "Building Properties"
          updateBuildState (\st => record
              { pattTitle = unwords [pattTitle st, "through",t]
              , pattDesc = pDesc
              } st)
          pure $ mkSolution bob d t d' ps'

solutionFromAST : SifBuilder impl
              -> (d : SifDomain)
              -> SifAST TySOLUTION
              -> DSLBuilder (SOLUTION impl d)
solutionFromAST bob d sast = do
    mkTimer (getSFName !getBuildState)
    startTimer (getSFName !getBuildState)
    sval <- buildSolutionE bob d sast
    stopTimer (getSFName !getBuildState)
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
patternFromFile bob pfile sfile = do
  putBuildState (defBuildSt pfile sfile)

  trace "Parsing Specifications"
  splitTimerMsg (unwords ["Building for ", pfile, "&", sfile])
                "Starting Parsing"

  (Right past) <- readSifFile problem pfile  | Left err => Sif.raise err

  splitTimerMsg (unwords ["Building for ", pfile, "&", sfile])
                "Finished problem parsing"

  (Right sast) <- readSifFile solution sfile | Left err => Sif.raise err
  splitTimerMsg (unwords ["Building for ", pfile, "&", sfile])
                "Finished solution parsing"

  trace "Comparing Context of Solution and Problem"
  case compatible past sast of
    Nothing => Sif.raise ContextMismatch
    Just d  => do
      p' <- problemFromAST bob d past

      splitTimerMsg (unwords ["Building for ", pfile, "&", sfile])
                   "Finished Problem now Solution"

      s' <- solutionFromAST bob d sast

      splitTimerMsg (unwords ["Building for ", pfile, "&", sfile])
                   "Finished Solution now Pattern"

      res <- buildPatternE bob d p' s'
      pure (d ** res)

-- --------------------------------------------------------------------- [ EOF ]
