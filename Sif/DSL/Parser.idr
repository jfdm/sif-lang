-- -------------------------------------------------------------- [ Parser.idr ]
-- Module    : Parser.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.DSL.Parser

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
import Lightyear.Strings

import Sif.Effs

import Sif.FileIO

import Sif.DSL.Parser.Problem
import Sif.DSL.Parser.Solution

import Sif.Types
import Sif.AbsSyntax
import Sif.Error

-- -------------------------------------------------------------- [ Directives ]

%default partial
%access public

-- ------------------------------------------------------------- [ File Reader ]

readSifFile : Parser a
           -> String
           -> Eff a SifEffs
readSifFile p fname = do
    trace $ unwords ["Parsing file:", show fname]
    src <- readFile fname
    case parse p src of
      Left err  => Sif.raise (ParseError fname err)
      Right res => pure res

-- --------------------------------------------------------------------- [ EOF ]
