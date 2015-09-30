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
import public Effect.Perf

import ArgParse

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
  bends   : List SifBackend
  builder : SifBackend

instance Default SifState where
  default = MkSifState
      defOpts
      defaultLib
      [backendAbsInterp,backendDirectRep]
      backendAbsInterp

-- -------------------------------------------------------------------- [ Effs ]

SifEffs : List EFFECT
SifEffs = [ FILE_IO ()
          , SYSTEM
          , STDIO
          , LOG
          , PERF
          , 'sif      ::: EXCEPTION SifError
          , 'bstate   ::: STATE BuildState
          , 'sstate   ::: STATE SifState
          ]

Sif : Type -> Type
Sif rTy = Eff rTy SifEffs

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

-- ----------------------------------------------------------------- [ Options ]

parseOptions : Sif SifOpts
parseOptions =
    case parseArgs defOpts convOpts !getArgs of
      Left err => Sif.raise $ GeneralError (show err)
      Right o  => pure o

-- --------------------------------------------------------------------- [ EOF ]
