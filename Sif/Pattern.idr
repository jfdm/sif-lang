||| Deep Modelling of Individual Patterns.
module Sif.Pattern

import public Sif.Pattern.Problem
import public Sif.Pattern.Solution


%access public

-- ------------------------------------------------------- [ Patern Definition ]

||| A Pattern is modelled as a problem solution pairing.
|||
||| Our pattern models are mapped to the GRL and valid pattern models
||| are valid GOal Graphs built using thr GRL.
|||
||| @m The abstract interpretation.
data Pattern : (m : GModel) -> Type where
  ||| Construct a pattern.
  MkPattern : (title : String)
           -> (sol   : Solution ss)
           -> (prob  : Problem p)
           -> Pattern (DList.foldr (insert) p ss)

-- --------------------------------------------------------------------- [ EOF ]
