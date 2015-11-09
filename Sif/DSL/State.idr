-- --------------------------------------------------------------- [ State.idr ]
-- Module    : State.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| State for Building Patterns from the AST.
module Sif.DSL.State

import Effects
import Effect.Default

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern
import Sif.Error

-- -------------------------------------------------------------- [ Directives ]
%access public
%default total

-- -------------------------------------------------------------------- [ Body ]

record BuildState where
  constructor MkSState
  getProbID        : Maybe String
  getRQs           : List (String, SifAST TyREQ)
  pattTitle        : String
  pattDesc         : Maybe String
  getPFName        : String
  getSFName        : String

defBuildSt : String -> String -> BuildState
defBuildSt p s = MkSState Nothing Nil "" Nothing p s

instance Default BuildState where
  default = defBuildSt "" ""

-- --------------------------------------------------------------------- [ EOF ]
