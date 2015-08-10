-- ---------------------------------------------------- [ ReferenceMonitor.idr ]
-- Module    : ReferenceMonitor.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Reference Monitor
module Sif.Lib.Solution.AccessControl.ReferenceMonitor

import Sif.Pattern

import Sif.Lib.Problem.PolicyEnforcement

-- -------------------------------------------------------------- [ Properties ]

namespace SinglePoint

   single_epoint : ADVANTAGE
   single_epoint = mkAdvantage "Enforces Policy" (Just desc) SATISFIED links
     where
       desc : String
       desc = """If all the requests are intercepted we can make sure that they comply with the rules"""

       links : TLINKS
       links = [mkLink MAKES enforcement, mkLink MAKES timely]

   choke : DISADVANTAGE
   choke = mkDisadvantage "Choke Point" (Just desc) DENIED links
     where
       desc : String
       desc = """Checking each request may result in intolerable performance loss. We may need to perform some checks at compile-time, for example, and not repeat them at execution time."""
       links : TLINKS
       links = [mkLink MAKES timely]

single_point : PROPERTY
single_point = mkProperty "Single Policy Enforcement Point" (Just desc) ps
  where
    desc : String
    desc = """There is a single point of enforcement at which all requests are analysed."""

    ps : TRAITS
    ps = [single_epoint,choke]

namespace Agnostic

    abstraction : ADVANTAGE
    abstraction = mkAdvantage "Agnostic" (Just desc) SATISFIED links
      where
        desc : String
        desc = """Implementation has not been constrained by using this abstract process."""

        links : TLINKS
        links = [mkLink MAKES agnostic]

    generalism : DISADVANTAGE
    generalism = mkDisadvantage "Specific Implementations" (Just desc) DENIED links
      where
        desc : String
        desc = """Specific implementations (concrete Reference Monitor) are needed for each type of resource. For example, a file manager is needed to control requests for files."""
        links : TLINKS
        links = [mkLink HURTS agnostic]

agnostic : PROPERTY
agnostic = mkProperty "Abstract Definition" (Just desc) ts
  where
    desc : String
    desc = """The solution described here is implementation agnostic."""

    ts : TRAITS
    ts = [abstraction, generalism]


-- ---------------------------------------------- [ Reference Monitor Solution ]

refmon : SOLUTION
refmon = mkSolution "Policy Enforcement Points" (Just desc) ps
  where
    desc : String
    desc = """Define an abstract process that intercepts all requests for resources and checks them for compliance with authorisations."""

    ps : PROPERTIES
    ps = [single_point, agnostic]

-- --------------------------------------------------------------------- [ EOF ]
