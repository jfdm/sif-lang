-- | Type System for our Sif Lang

module TypeSystem
    ( module TypeSystem.Types,
      module TypeSystem.RulesAssoc,
      module TypeSystem.RulesSpecial,
      module TypeSystem.RulesRealise,
      module TypeSystem.RulesAggre
    ) where

import TypeSystem.Types hiding (Pattern', Relation')
import TypeSystem.RulesAssoc
import TypeSystem.RulesSpecial
import TypeSystem.RulesRealise
import TypeSystem.RulesAggre
