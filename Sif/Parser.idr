-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser

import public Lightyear
import public Lightyear.Strings

import public Effects
import public Effect.File
import public Effect.Exception
import public Effect.State

import public Sif.Parser.Problem
import public Sif.Parser.Pattern

import Sif.Pattern
import public Sif.Parser.File
import public Sif.Parser.State

%default partial
%access public

problemFromFile : String
               -> Eff PROBLEM
                      [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
problemFromFile s = do
    past <- readSifFile problem s
    rval <- getProblemEff past
    pure $ rval
  where
    conRQ : RTy -> String -> Maybe String -> REQUIREMENT
    conRQ FUNC s d = mkFunctional s d
    conRQ USAB s d = mkUsability s d
    conRQ RELI s d = mkReliability s d
    conRQ PERF s d = mkPerformance s d
    conRQ SUPP s d = mkSupportability s d

    doRQ : ProbAST ReqTy
        -> Eff REQUIREMENT [EXCEPTION String, 'bst ::: STATE BuildEnv]
    doRQ (MkReq i ty t d) = do
      st <- 'bst :- get
      case lookup i (getRQs st) of
        Just x  => raise "Identifier already Exists"
        Nothing => do
          let r = conRQ ty t d
          'bst :- put (record { getRQs = (i,r) :: (getRQs st)} st)
          pure r

    doRQs : List (ProbAST ReqTy)
         -> Eff (List REQUIREMENT)
                [EXCEPTION String, 'bst ::: STATE BuildEnv]
    doRQs xs = do
      rval <- mapE (\x => doRQ x) xs
      pure rval


    getProblemEff : ProbAST ProbTy
                 -> Eff PROBLEM [EXCEPTION String, 'bst ::: STATE BuildEnv]
    getProblemEff (MkProb i t d rs) = do
        rs' <- doRQs rs
        let p = mkProblem t d rs'
        'bst :- update (\st => record {getProb = (i, p)} st)
        pure p

solutionFromFile : String -> Eff SOLUTION [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
solutionFromFile f = do
    (pt, sast) <- readSifFile solution f
    'bst :- update (\st => record {getPData = pt} st)
    sval <- buildSolution sast
    pure sval
  where
    buildAffect : SolAST AffectTy
               -> Eff TLINK [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
    buildAffect (Affects c i) = do
      st <- 'bst :- get
      case lookup i (getRQs st) of
        Nothing => raise "Identifier missing"
        Just r  => pure $ mkLink c r

    buildTrait : SolAST TraitTy
              -> Eff TRAIT [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
    buildTrait (Trait ty t v d as) = do
      as' <- mapE (\x => buildAffect x) as
      case ty of
        ADV => pure $ mkAdvantage t d v as'
        DIS => pure $ mkDisadvantage t d v as'

    buildProperty : SolAST PropTy
              -> Eff PROPERTY [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
    buildProperty (Property t d ts) = do
      ts' <- mapE (\x => buildTrait x) ts
      pure $ mkProperty t d ts'

    buildSolution : SolAST SolTy
                -> Eff SOLUTION [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
    buildSolution (Solution t pID d ps) = do
      st <- 'bst :- get
      let pID' = fst $ getProb st
      if not (pID' == pID)
        then raise "The paired problem doesn't exist."
        else do
          ps' <- mapE (\p => buildProperty p) ps
          pure $ mkSolution t d ps'

buildPattern : String
            -> String
            -> Eff PATTERN [FILE_IO (), EXCEPTION String, 'bst ::: STATE BuildEnv]
buildPattern p s = do
  'bst :- put (mkDefState)
  p' <- problemFromFile p
  s' <- solutionFromFile s
  st <- 'bst :- get
  pure $ mkPattern (fst $ getPData st) (snd $ getPData st) p' s'


-- --------------------------------------------------------------------- [ EOF ]
