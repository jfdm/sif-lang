module Sif.Parser.State

import Sif.Pattern
import Sif.Parser.Problem
import Sif.Parser.Pattern

import Effect.Default

record BuildState where
  constructor MkSState
  getProb  : (String, Maybe PROBLEM)
  getRQs   : List (String, REQUIREMENT)
  pattTitle : String
  pattDesc  : Maybe String

defBuildSt : BuildState
defBuildSt = MkSState ("", Nothing) Nil "" Nothing

instance Default BuildState where
  default = defBuildSt


-- --------------------------------------------------------------------- [ EOF ]
