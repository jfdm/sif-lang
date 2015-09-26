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
  getProb     : Maybe String
  getRQs      : List (String, SifAST TyREQ)
  pattTitle   : String
  pattDesc    : Maybe String
  getPFName   : String
  getSFName   : String
  getDomainID : Maybe String
  getDomain   : SifDomain

defBuildSt : String -> String -> BuildState
defBuildSt p s = MkSState Nothing Nil "" Nothing p s Nothing defaultDomain

instance Default BuildState where
  default = defBuildSt "" ""

-- --------------------------------------------------------------------- [ EOF ]
