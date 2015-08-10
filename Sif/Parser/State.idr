module Sif.Parser.State

import Sif.Pattern
import Sif.Parser.Problem
import Sif.Parser.Pattern

import Effect.Default

record BuildEnv where
  constructor MkSState
  getProb  : (String, Maybe PROBLEM)
  getRQs   : List (String, REQUIREMENT)
  getPData : (String, Maybe String)

defBuildSt : BuildEnv
defBuildSt = MkSState ("", Nothing) Nil ("", Nothing)

instance Default BuildEnv where
  default = defBuildSt


-- --------------------------------------------------------------------- [ EOF ]
