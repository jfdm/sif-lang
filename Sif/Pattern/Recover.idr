-- ------------------------------------------------------------- [ Recover.idr ]
-- Module    : Recover.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern.Recover

import Data.AVL.Dict
import Data.Sigma.DList
import GRL.Lang.GLang

import XML.DOM

import Edda
import Edda.Writer.Org

import Freyja

import Sif.Types
import Sif.Pattern.Model
import Sif.Pattern.API

%access private

blocks : Maybe EddaBody -> Maybe String
blocks Nothing  = Nothing
blocks (Just b) = Just $ concatMap block b

-- -------------------------------------------------------------------- [ Reqs ]

recoverReq : SifBuilder impl
           -> (d : SifDomain)
           -> Requirement ty
           -> REQUIREMENT impl d
recoverReq bob c r {ty} =
    mkRequirement bob c ty (inlines $ name r) (blocks $ Just (desc r))

-- ----------------------------------------------------------------- [ Problem ]

recoverProblem : SifBuilder impl
              -> (d : SifDomain)
              -> Problem
              -> PROBLEM impl d
recoverProblem bob c p = mkProblem bob c t d rs
  where
    t : String
    t = inlines (name p)

    d : Maybe String
    d = blocks (Just (desc p))

    rs : REQUIREMENTS impl c
    rs = mapDList (\r => recoverReq bob c r) (reqs p)

-- ------------------------------------------------------------------ [ Affect ]

recoverAffect : SifBuilder impl
             -> (d : SifDomain)
             -> Affect
             -> AFFECT impl d
recoverAffect bob c a = mkAffect bob c (cval a) r d
  where
    r : REQUIREMENT impl c
    r = recoverReq bob c (req a)

    d : Maybe String
    d = blocks (desc a)

-- ------------------------------------------------------------------- [ Trait ]

recoverTrait : SifBuilder impl
            -> (d : SifDomain)
            -> Trait ty
            -> TRAIT impl d
recoverTrait bob c t {ty} = mkTrait bob c ty n d (sval t) as
  where
    n : String
    n = inlines (name t)

    d : Maybe String
    d = blocks (Just (desc t))

    as : AFFECTS impl c
    as = map (\a => recoverAffect bob c a) (affects t)

-- ---------------------------------------------------------------- [ Property ]

recoverProperty : SifBuilder impl
               -> (d : SifDomain)
               -> Property
               -> PROPERTY impl d
recoverProperty bob c p = mkProperty bob c n d ts
  where
    n : String
    n = inlines (name p)

    d : Maybe String
    d = blocks (Just (desc p))

    ts : TRAITS impl c
    ts = mapDList (\t => recoverTrait bob c t) (traits p)

-- ---------------------------------------------------------------- [ Solution ]

recoverSolution : SifBuilder impl
               -> (d : SifDomain)
               -> Solution
               -> SOLUTION impl d
recoverSolution bob c s = mkSolution bob c n d ps
  where
    n : String
    n = inlines (name s)

    d : Maybe String
    d = blocks (Just (desc s))

    ps : PROPERTIES impl c
    ps = map (\p => recoverProperty bob c p) (properties s)

-- -------------------------------------------------------------------- [ Main ]

public
fromFreya : SifBuilder impl
         -> PatternDoc
         -> (d ** PATTERN impl d)
fromFreya bob doc {impl} =
    (c ** mkPattern bob c t d p s)
  where
    t : String
    t = inlines (name doc)

    d : Maybe String
    d = blocks (Just (summary doc))

    c' : Context
    c' = (context doc)

    c : SifDomain
    c = MkDomain (inlines $ name c') (blocks (Just (desc c')))

    p : PROBLEM impl c
    p = recoverProblem bob c (problem doc)

    s : SOLUTION impl c
    s = recoverSolution bob c (solution doc)

-- --------------------------------------------------------------------- [ EOF ]
