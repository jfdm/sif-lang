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
readSifFile p f = do
    trace $ unwords ["Reading file:", f]
    case !(open f Read) of
      True => do
        src <- readAcc ""
        close
        trace "Parsing File"
        case parse p src of
          Left err  => Sif.raise (ParseError f err)
          Right res => pure res
      False => Sif.raise (FileMissing f)
  where
    readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
    readAcc acc = if (not !(eof))
                     then readAcc (acc ++ !(readLine))
                     else pure acc

-- --------------------------------------------------------------------- [ EOF ]
