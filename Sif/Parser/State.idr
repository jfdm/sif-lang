module Sif.Parser.State

import Sif.Pattern
import Sif.Parser.Problem
import Sif.Parser.Pattern

import Effects
import Effect.Default

record SifState where
  constructor MkSState
  getProb : (String, PROBLEM)
  getRQs  : List (String, REQUIREMENT)
  getPData : (String, Maybe String)

instance Default SifState where
  default = MkSState ("", mkProblem "" Nothing Nil) Nil ("", Nothing)

mkDefState : SifState
mkDefState = MkSState ("", mkProblem "" Nothing Nil) Nil ("", Nothing)

-- --------------------------------------------------------------------- [ EOF ]
