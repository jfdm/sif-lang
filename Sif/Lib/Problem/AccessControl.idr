-- ------------------------------------------------------- [ AccessControl.idr ]
-- Module    : AccessControl.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Problem definition for Access Control
|||
||| This definition combines the problem definitions for
||| Authorisation, RBAC, MBAC.
module Sif.Lib.Problem.AccessControl

import Sif.Pattern

-- Combination of Authorisation and MBAC

-- ------------------------------------------------------------ [ Requirements ]

control : FUNCTIONAL
control = mkFunctional "Access Restriction" (Just desc)
  where
    desc : String
    desc = "Access must be restricted to resources."

independance : FUNCTIONAL
independance = mkFunctional "Application Independence" (Just desc)
  where
    desc : String
    desc = "The authorization structure must be independent of the type of resources. For example, it should describe access by users to conceptual entities, access by programs to operating system resources, etc., in a uniform way."

flexible : FUNCTIONAL
flexible = mkFunctional "Flexibility" (Just desc)
  where
    desc : String
    desc = "The authorisation structure should be flexible enough to accommodate different types of subjects, objects, and rights."

ease_of_adjust : FUNCTIONAL
ease_of_adjust = mkFunctional "Easy Adjustment" (Just desc)
  where
    desc : String
    desc = "It should be easy to modify the rights of a subject in response to changes in their duties or responsibilities."

simple_admin : FUNCTIONAL
simple_admin = mkFunctional "Ease of Administration" (Just desc)
  where
    desc : String
    desc = "The administration of authorizations should be simplified. In addition, in open systems such as web portals we usually don’t know the subjects in advance. Access may also be dependent on values of the object; for example, a patient can access her own record."

-- ------------------------------------------------------ [ Problem Definition ]

accessControl : PROBLEM
accessControl = mkProblem "Access Control" (Just desc)
    [independance, flexible, ease_of_adjust, simple_admin]
  where
    desc : String
    desc = """In computational environments entities make requests for data or resources. In these environments these entities will have prescribed restrictions on the data and resources they can access. There needs to be a means to control access to resources, including information. The first step is to declare who is authorised to access resources in specific ways. Otherwise, any active entity (user, process) could access any resource and there would be confidentiality and integrity problems.

How do we describe who is authorised to access specific resources in a
system?"""

-- --------------------------------------------------------------------- [ EOF ]
