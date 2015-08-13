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

import public ArgParse

import Sif.Pattern
import Sif.Parser.State
import Sif.Error
import Sif.Library
import Sif.Options

%access public

record SifState where
  constructor MkSifState
  opts : SifOpts
  lib  : SifLib
  benv : BuildState

instance Default SifState where
  default = MkSifState defOpts defaultLib defBuildSt

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , SYSTEM
          , STDIO
          , 'sif      ::: EXCEPTION SifError
          , 'argparse ::: EXCEPTION ArgParseError
          , 'sstate   ::: STATE SifState
          ]

namespace Sif
  raise : SifError -> Eff b ['sif ::: EXCEPTION SifError]
  raise err = 'sif :- Exception.raise err


fromJustEff : Maybe a -> Eff a ['sif ::: EXCEPTION SifError]
fromJustEff (Just x) = pure x
fromJustEff Nothing = Sif.raise InternalErr

getOptions : Eff SifOpts ['sstate ::: STATE SifState]
getOptions = pure $ opts !('sstate :- get)

putOptions : SifOpts -> Eff () ['sstate ::: STATE SifState]
putOptions o = 'sstate :- update (\st => record {opts = o} st)

updateOptions : (SifOpts -> SifOpts) -> Eff () ['sstate ::: STATE SifState]
updateOptions f = 'sstate :- update (\st => record {opts = f (opts st)} st)

getLibrary : Eff SifLib ['sstate ::: STATE SifState]
getLibrary = pure $ lib !('sstate :- get)

putLibrary : SifLib -> Eff () ['sstate ::: STATE SifState]
putLibrary l = 'sstate :- update (\st => record {lib = l} st)

updateLibrary : (SifLib -> SifLib) -> Eff () ['sstate ::: STATE SifState]
updateLibrary f = 'sstate :- update (\st => record {lib = f (lib st)} st)

getBuildState : Eff BuildState ['sstate ::: STATE SifState]
getBuildState = pure $ benv !('sstate :- get)

putBuildState : BuildState -> Eff () ['sstate ::: STATE SifState]
putBuildState l = 'sstate :- update (\st => record {benv  = l} st)

updateBuildState : (BuildState -> BuildState) -> Eff () ['sstate ::: STATE SifState]
updateBuildState f = 'sstate :- update (\st => record {benv = f (benv st)} st)

parseOptions : Eff SifOpts SifEffs
parseOptions = parseArgs defOpts convOpts !getArgs

-- --------------------------------------------------------------------- [ EOF ]
