-- ---------------------------------------------------------------- [ ABAC.idr ]
-- Module    : ABAC.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| The pattern for ABAC.
module Pattern.AccessControl.ABAC

import Sif.Pattern

import Problem.AccessControl


{-

#+TITLE: Metadata-based Access Control (MBAC) Pattern

* Also Known as
Attributed-Based Access Control

* Intent
Control access based on properties of subjects or objects.

* Context
Any environment where we need to control access to comput-
ing resources and where some users may not be pre-registered.

* Problem
Similarly to the other patterns in section 4, permissions for
subjects accessing security objects have to be described in a suitable way.
The administration of authorizations should be simplified as done in
Role-based Access Control. In addition, in open systems such as web
portals we usually don’t know the subjects in advance. Access may also
be dependent on values of the object; for example, a patient can access
her own record.

* Solution
Subjects in requests and actual requested objects are repre-
sented as sets of attribute or property values. In addition, we need to rep-
resent authorization subjects and objects as sets of predicates or asser-
tions on attribute or property values, i.e. the authorizations are not de-
fined directly between subjects and objects but between so called subject
and object descriptors. A subject descriptor consists of several attribute
conditions (e.g. age > 21, ZIP code beginning with “93”) which can pos-
sibly correspond to several real subjects. The same holds for the object
descriptors, where conditions are defined on object properties (e.g. re-
lated to a certain project, released by a certain publisher). As a conse-
quence, subject and object descriptors are something like subject and ob-
ject groups, however, not explicitly grouped by an administrator, but im-
plicitly by their attribute or property values.

#+name: mbac-struct
#+begin_src plantuml :file mbac-structure.png
@startuml

class Attribute
class EnvAttr as "Environment Attribute"
class SubjectAttr as "Subject Attribute"
class ResAttr as "Resource Attribute"

class Condition
class Authorisation

class Subject
class SubjectDesc
class SubjectQual
class SubjectAttrValue as "Subject Attribute Value"

class Resource
class ResDesc
class ResQual
class ResAttrValue as "Resource Attribute Value"

SubjectDesc "*" -right- "*" ResDesc : isAuthorisedFor
(SubjectDesc, ResDesc) .up. Authorisation

Attribute "*" -up- "*" Condition
Authorisation *-down- "*" Condition

Attribute <|-- EnvAttr
Attribute <|-left- SubjectAttr
Attribute <|-right- ResAttr

SubjectDesc *-- "*" SubjectQual
SubjectQual "*" -- "1" SubjectAttr
SubjectAttr "1" -- "*" SubjectAttrValue
SubjectAttrValue "1" --* Subject

ResDesc *-- "*" ResQual
ResQual "*" -- "1" ResAttr
ResAttr "1" -- "*" ResAttrValue
ResAttrValue "1" --* Resource

@enduml
#+end_src

#+CAPTION: MBAC Structure
#+ATTR_LaTeX: :width 0.5\textwidth
#+RESULTS: mbac-struct
[[file:mbac-structure.png]]

Figure 5 shows the elements of the MBAC model in form of a class dia-
gram. This is a composite pattern that uses the Authorization Pattern. The
class Subject describes the actual accessing entity. A subject is described
by several attributes values (as instances of the AttributeValue class). The
class Attribute denotes the attribute schema (e.g. an instance of the class
would be age). A similar decoupling has been done for the class Object.
The object represents the resource which has to be protected and is de-
scribed by property values, represented by the class PropertyValue.
Rights are defined between subject and object descriptors (represented by
the SubjectDescriptor and ObjectDescriptor classes). Like the attribute
and property values the assertions that define the descriptors have been
decoupled into the AttributeQualifier and PropertyQualifier classes.

* Consequences
By using subject attributes and object properties for the
definition of authorizations, administration is simplified and flexibility is
improved. When changing attribute values, affected permissions will be
automatically updated without the need to explicitly change role defini-
tions. Subjects can be roles as well as users or processes.

-}


abac : PATTERN
abac = mkPattern "Access Control using Attribute Based Access Control"
                 (Just desc) accessControl solution
  where
    desc : String
    desc = """This pattern presents the provision of access control based upon the analysis of attributes """


-- --------------------------------------------------------------------- [ EOF ]
