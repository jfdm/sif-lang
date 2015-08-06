-- --------------------------------------------------- [ PolicyEnforcement.idr ]
-- Module    : PolicyEnforcement.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| The problem of enforcing access control.
module Problem.PolicyEnforcement

import Sif.Pattern

-- ------------------------------------------------------- [ Forces Definition ]

enforcement : FUNCTIONAL
enforcement = mkFunctional "Policy Enforcement" (Just desc)
  where
    desc : String
    desc = """Definition of authorisation policies is not sufficient, their enforcement must also be ensured."""

timely : FUNCTIONAL
timely = mkFunctional "Timely Policy Enforcement" (Just desc)
  where
    desc : String
    desc = """The enforcement of a policy must occur at the same time access to a resource occurs"""

agnostic : FUNCTIONAL
agnostic = mkFunctional "Tech Agnostic" (Just desc)
  where
    desc : String
    desc = """There are many possible ways of enforcement, depending on the specific architectural unit or level involved. We need an abstract model of enforcement that applies to every level of the system."""

-- ------------------------------------------------------ [ Problem Definition ]

policyEnforcement : PROBLEM
policyEnforcement = mkProblem "Policy Enforcement Point" (Just desc) reqs
  where
    reqs : REQUIREMENTS
    reqs = [enforcement, timely, agnostic]

    desc : String
    desc = """In computational environments entities make requests for data or resources. In these environments these entities will have prescribed restrictions on the data and resources they can access. If the authorisation rights are not enforced then these entities may perform all types of illegal actions. The problem is how to enforce these authorisations."""


-- --------------------------------------------------------------------- [ EOF ]
