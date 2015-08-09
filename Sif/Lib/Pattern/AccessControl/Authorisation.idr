module Sif.Lib.Pattern.AccessControl.Authorisation

import Sif.Pattern

{-
#+TITLE: Authorisation
#+BIBLIOGRAPHY: ../bibliography plain limit:t

#+begin_abstract
This pattern describes who is authorised to access specific resources in
a system, in an environment in which we have resources whose access
needs to be controlled. It indicates, for each active entity that can
access resource, which resources it can access, and how it can access
them.
#+end_abstract

* Example

In a medical information system we keep sensistive information about
patients. Unrestricted disclosure of this data would violate the privact
of the patients, while unrestricted modification could jeopardise the
health of the patients.

* Context

Any environment in which we have resources whose access needs to be
controlled.

* Problem

We need to have a way to control access to resources, including
information. The first step is to declare who is authorised to access
resources in specific ways. Otherwise, any active entity (user, process)
could access any resource and we could have confidentiality and
integrity problems.

How do we describe who is authorised to access specific resources in a
system?

* Forces

The solution to this problem must balance the following resources:

The authorization structure must be independent of the type of
resources. For example, it should describe access by users to conceptual
entities, access by programs to operating system resources, etc., in a
uniform way.

The authorisation structure should be flexible enough to accommodate
different types of subjects, objects, and rights.

It should be easy to modify the rights of a subject in response to
changes in their duties or responsibilities.

* Solution

Indicate, for each active entity that can access resources, which
resources it can access and how.

** Structure

The =Subject()= class describes an active entity that attempts to access
a resource (=ProtectionObject=) in some way. The =ProtectionObject()=
class represents the resource to be protected. The association between
the subject and object defines an authorisation, from which the pattern
gets its name. The association class =Right()= describes the access type
(for example, read, write) the subject is allowed to perform on the
corresponding object. Through this class one can check the rights that a
subject has on some object, or who is allowed to access a given object.

The figure below shows the elements of an authorisation in form if a
class diagram.

#+name: auth-structure
#+begin_src plantuml :file auth-structure.png
@startuml
class Subject {
  + id
}

class ProtectionObject {
  + id
}

class Right {
  + access_type
  + checkRights()
}

(Subject, ProtectionObject) .. Right

@enduml
#+end_src

#+CAPTION: Authorisation Structure
#+ATTR_LaTeX: :width 0.5\textwidth
#+RESULTS: auth-structure

** Dynamics

Possible dynamics are shown below.

#+name: auth-dynamics
#+begin_src plantuml :file auth-dynamics.png
@startuml

participant Entity
participant "Reference\n Monitor" as refmon
participant Right

Entity -> refmon : request
refmon -> Right : checkRights()
Right -> refmon : decision
refmon -> Entity : decision

@enduml
#+end_src

#+CAPTION: Authorisation Dynamics
#+ATTR_LaTeX: :width 0.5\textwidth
#+RESULTS: auth-dynamics

* Implementation

An organisation, according to its policies, should define all the
required accesses to resources. The most common policy is need-to-know,
in which active entities receive access rights according to their needs.

This pattern is abstract and there are many implementatios: the two most
common approaches are Access Control Lists and Capabilities. Access
Control Lists (ACLs) are kept with the objects to indicate who is
authorised to access them, while Capabilities are assigned to processes
to define their execution rights. Access types should be application
oriented.

* Example Resolved

A hospital using an authorisation system can define rules that allow
only doctors or nurses to modify patient records, and only medical
personnel to read patient records. This approach allows only qualified
personnel to read and modify records.

* Variant

The full access matrix model usually described in textbooks also
includes:

-  Predicates, or guards, which may restrict the use of authorisation
   according to specific conditions.

-  Delegation of some of the authorisations by their holders to other
   subjects through the use of a Boolean 'copy' flag.

The next figure extends Authorisation to include those aspects. The
class Right now includes not only the type of access allowed, but also a
predicate that must be true for the authorisation to hold, and a copy
flag that can be true or false, indicating whether or no tthe right can
be transferred. CheckRights is an operation to determine the rights of a
subject or to find who has the rights to access a given object.

#+name: auth-ext-structure
#+begin_src plantuml :file auth-ext-structure.png
@startuml

class Subject {
  + id
}

class ProtectionObject {
  + id
}

class Right {
  + access_type
  + predicate
  + copy_flag
  + checkRights()
}

(Subject, ProtectionObject) .. Right

@enduml
#+end_src

#+CAPTION: Authorisation Extended Structure
#+ATTR_LaTeX: :width 0.5\textwidth
#+RESULTS: auth-ext-structure
[[file:auth-structure.png]]

* Known uses

This pattern defines the most basic type of authorisation rule, on which
most complex access control models are based. It is based on the concept
of access matrix, a fundamental security model \autocite{Sum97}. Its first
object-oriented form appeared in \autocite{Fer93}. Subsequently, it has appeared
in several other papers and products \autocite{Ess97,Kod01}. It is the basis
for the access control systems of most commercial products such as Unix,
Windows, Oracle, and many others. The Packet Filter Firewall pattern
implements a variety of this pattern in which the subjects and objects
are defined by Internet addresses.

* Consequences

The following benefits may be expected from applying this pattern:

-  The pattern applies to any type of resource. Subjects can be
   executing processes, users, roles, user groups. Protection objects
   can be transactions, memory areas, I/O devices, files, or other OS
   resources. Access types can be read, write, execute, or methods in
   higher-level objects.

-  It is convenient to add or remove authorisations.

-  Some systems separate administrative authorisations from user
   authorisations for further security, on the principle of separation
   of duties \autocite{Woo79}.

-  The request may not need to specify the exact object in the rule,
   this object may be implied by an existing protected object \autocite{Fer75}.
   Subjects and acess types may also be implied. This improves
   flexibility at the cost of some extra processing time (to deduce the
   specific rule needed).

The following potential liabilities may arise from applying this
pattern:

-  If there are many users or many objects, a large number of rules must
   be written.

-  It may be hard for the security administrator to realise why a given
   subject needs a right, or the implications of a new rule.

-  Defining authorisation rules is not enough, we also need an
   enforcement mechanism.

* Related patterns

The Role-Based Access Control pattern is a specialisation of this
pattern. The Reference Monitor pattern complements this pattern by
defining how to enforce the defined rights.
-}
