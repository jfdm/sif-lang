||| Deep Modelling of Individual Patterns.
module Sif.Pattern

import public Sif.Pattern.Problem
import public Sif.Pattern.Solution

data Pattern : GModel -> Type where
  MkPattern : (title : Maybe String)
           -> (sol   : Solution ss)
           -> (prob  : Problem p)
           -> Pattern (DList.foldr (insert) p ss)
