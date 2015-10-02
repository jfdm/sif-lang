-- -------------------------------------------------------------- [ Parser.idr ]
-- Module    : Parser.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.DSL.Parser

-- ----------------------------------------------------------------- [ Imports ]
import Effects
import Effect.File
import Effect.Logging.Default

import Lightyear
import Lightyear.Strings
import Lightyear.StringFile

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
           -> Eff (Either SifError a) [FILE_IO (), LOG]
readSifFile p fname = do
    trace $ unwords ["Parsing file:", show fname]
    res <- parseFile FileMissing ParseError p fname
    case res of
      Left err => pure $ Left err
      Right  r => pure $ Right r

-- --------------------------------------------------------------------- [ EOF ]
