-- --------------------------------------------------------------- [ State.idr ]
-- Module    : State.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| State for Building Patterns from the AST.
module Sif.DSL.State

import Effect.Default

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern

-- -------------------------------------------------------------- [ Directives ]
%access public
%default total

-- -------------------------------------------------------------------- [ Body ]

record BuildState where
  constructor MkSState
  getProb  : Maybe String
  getRQs   : (List (String, SifAST tyREQ))
  pattTitle : String
  pattDesc  : Maybe String

defBuildSt : BuildState
defBuildSt = MkSState Nothing Nil "" Nothing

instance Default BuildState where
  default = defBuildSt

-- --------------------------------------------------------------------- [ EOF ]
