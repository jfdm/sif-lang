-- ------------------------------------------------------------- [ Pattern.idr ]
-- Module    : Pattern.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Prelude.Pattern

import Sif.Pattern
import Sif.Prelude.Problem
import Sif.Prelude.Solution

-- ------------------------------------------------------ [ Pattern Definition ]

referenceMonitor : PATTERN
referenceMonitor = mkPattern "Policy Enforcement through Reference Monitoring" (Just desc)
     policyEnforcement
     refmon
   where
     desc : String
     desc = """In a computational environment in which users or processes make requests for data or resources, this pattern enforces declared access restrictions when an active entity requests resources. It describes how to define an abstract process that intercepts all requests for resources and checks them for compliance with authorisations."""


infoSecAsymCrypto : PATTERN
infoSecAsymCrypto = mkPattern "Information Security using Asymmetric Crypto" Nothing infosec asymCrypto

infoSecSymmCrypto : PATTERN
infoSecSymmCrypto = mkPattern
    "Information Security using Symmetric Crypto"
    Nothing
    infosec
    symCrypto


-- --------------------------------------------------------------------- [ EOF ]
