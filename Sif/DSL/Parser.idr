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
    case !(readFile FileMissing fname) of
      Left err  => pure $ Left err
      Right src => do
        case parse p src of
          Left err  => pure $ Left (ParseError fname err)
          Right res => pure $ Right res

-- --------------------------------------------------------------------- [ EOF ]
