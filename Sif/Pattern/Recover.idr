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



public
fromFreya : SifBuilder impl
         -> PatternDoc
         -> (d ** PATTERN impl d)
fromFreya bob doc {impl} =
    mkPattern bob c t d (getProof p) (getProof s)
  where
    t : String
    t = inlines (name doc)

    d : String
    d = concatMap block (summary doc)

    c' : Context
    c' = (context doc)

    c : SifDomain
    c = MkDomain (inlines name c) ()

    p : (d ** PROBLEM impl d)

-- --------------------------------------------------------------------- [ EOF ]
