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
import public Effect.Logging.Default

import public ArgParse

import Sif.Types
import Sif.DSL.State
import Sif.Error
import Sif.Pattern
import Sif.Builder.AbsInterp
import Sif.Builder.DirectRep
import Sif.Library
import Sif.Options

-- -------------------------------------------------------------- [ Directives ]
%access public

-- -------------------------------------------------------------- [ State Defs ]

record SifState where
  constructor MkSifState
  opts    : SifOpts
  lib     : SifLib
  benv    : BuildState
  bends   : List SifBackend
  builder : SifBackend

instance Default SifState where
  default = MkSifState
      defOpts
      defaultLib
      defBuildSt
      [backendAbsInterp,backendDirectRep]
      backendAbsInterp

-- -------------------------------------------------------------------- [ Effs ]

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , SYSTEM
          , STDIO
          , LOG
          , 'sif      ::: EXCEPTION SifError
          , 'argparse ::: EXCEPTION ArgParseError
          , 'sstate   ::: STATE SifState
          ]

-- -------------------------------------------------------- [ Helper Functions ]

namespace Sif
  raise : SifError -> Eff b ['sif ::: EXCEPTION SifError]
  raise err = 'sif :- Exception.raise err


fromJustEff : Maybe a -> Eff a ['sif ::: EXCEPTION SifError]
fromJustEff (Just x) = pure x
fromJustEff Nothing = Sif.raise InternalErr

-- ----------------------------------------------------------------- [ Options ]

getOptions : Eff SifOpts ['sstate ::: STATE SifState]
getOptions = pure $ opts !('sstate :- get)

putOptions : SifOpts -> Eff () ['sstate ::: STATE SifState]
putOptions o = 'sstate :- update (\st => record {opts = o} st)

updateOptions : (SifOpts -> SifOpts) -> Eff () ['sstate ::: STATE SifState]
updateOptions f = 'sstate :- update (\st => record {opts = f (opts st)} st)


-- ----------------------------------------------------------------- [ Library ]
getLibrary : Eff SifLib ['sstate ::: STATE SifState]
getLibrary = pure $ lib !('sstate :- get)

putLibrary : SifLib -> Eff () ['sstate ::: STATE SifState]
putLibrary l = 'sstate :- update (\st => record {lib = l} st)

updateLibrary : (SifLib -> SifLib) -> Eff () ['sstate ::: STATE SifState]
updateLibrary u = 'sstate :- update (\st => record {lib = u (lib st)} st)


-- ---------------------------------------------------------------- [ Backends ]

getSifBackend : Eff SifBackend ['sstate ::: STATE SifState]
getSifBackend = pure $ (builder !('sstate :- get))

setSifBackend : Maybe String -> Eff () SifEffs
setSifBackend Nothing = do
    putStrLn $ unwords ["Using Default Backend"]
    'sstate :- update (\st => record {builder = backendAbsInterp} st)
setSifBackend (Just n) = do
    st <- 'sstate :- get
    case find (\x => name x == n) (bends st) of
      Nothing => do
        printLn (NoSuchBackend n)
        setSifBackend Nothing
      Just x  => do
        putStrLn $ unwords ["Using backend", show n]
        'sstate :- update (\st => record {builder = x} st)

addSifBackend : SifBackend -> Eff () ['sstate ::: STATE SifState]
addSifBackend b = 'sstate :- update (\st => record {bends = (b::bends st)} st)

-- ----------------------------------------------------------- [ Build Helpers ]

getBuildState : Eff BuildState ['sstate ::: STATE SifState]
getBuildState = pure $ benv !('sstate :- get)

putBuildState : BuildState -> Eff () ['sstate ::: STATE SifState]
putBuildState l = 'sstate :- update (\st => record {benv  = l} st)

updateBuildState : (BuildState -> BuildState) -> Eff () ['sstate ::: STATE SifState]
updateBuildState f = 'sstate :- update (\st => record {benv = f (benv st)} st)

-- ----------------------------------------------------------------- [ Options ]
parseOptions : Eff SifOpts SifEffs
parseOptions = parseArgs defOpts convOpts !getArgs

-- --------------------------------------------------------------------- [ EOF ]
