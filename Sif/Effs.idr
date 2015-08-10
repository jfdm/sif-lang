-- ---------------------------------------------------------------- [ Effs.idr ]
-- Module    : Effs.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Effs

import public Effects
import public Effect.System
import public Effect.State
import public Effect.Exception
import public Effect.File
import public Effect.StdIO

import public Sif.Pattern
import public Sif.Parser.State
import public Sif.Error
import public Sif.Lib

import public ArgParse

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , 'sif ::: EXCEPTION SifError
          , 'argparse ::: EXCEPTION ArgParseError
          , SYSTEM
          , STDIO
          , 'lib ::: STATE SifLib
          , 'bst ::: STATE BuildEnv
          ]

namespace Sif
  raise : SifError -> Eff b ['sif ::: EXCEPTION SifError]
  raise err = 'sif :- Exception.raise err



fromJustEff : Maybe a -> Eff a ['sif ::: EXCEPTION SifError]
fromJustEff (Just x) = pure x
fromJustEff Nothing = Sif.raise InternalErr

-- --------------------------------------------------------------------- [ EOF ]
