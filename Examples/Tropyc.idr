-- -------------------------------------------------------------- [ Tropyc.idr ]
-- Module    : Tropyc.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Example.Tropyc

{-
import Sif.PLang

namespace Aim
  confidentiality : FUNCTIONAL
  confidentiality = mkFunctional "Confidentiality"

  msgIntegrity : FUNCTIONAL
  msgIntegrity = mkFunctional "Message Integrity"

  seshIntegrity : FUNCTIONAL
  seshIntegrity = mkFunctional "Session Integrity"

  dataAuthenticity : FUNCTIONAL
  dataAuthenticity = mkFunctional "Data Authenticity"

  subjectAuthenticity : FUNCTIONAL
  subjectAuthenticity = mkFunctional "Subject Authenticity"

  originNonRepudiation : FUNCTIONAL
  originNonRepudiation = mkFunctional "Non-Repudiation of Origin"

  receiptNonRepudiation : FUNCTIONAL
  receiptNonRepudiation = mkFunctional "Non-Repudiation of Receipt"

  transNonRepudiation : FUNCTIONAL
  transNonRepudiation = mkFunctional "Non-Repudiation of Transmission"

aims : List (FUNCTIONAL)
aims = [confidentiality,
        msgIntegrity, seshIntegrity,
        dataAuthenticity, subjectAuthenticity,
        originNonRepudiation, receiptNonRepudiation, transNonRepudiation]

namespace Pattern
  mpatt : GENERIC
  mpatt = mkGeneric "Cryptographic MetaPattern"

  infosec : COMPONENT
  infosec = mkComponent "Information Secrecy"

  mauth : COMPONENT
  mauth = mkComponent "Message Authentication"

  sauth : COMPONENT
  sauth = mkComponent "Sender Authentication"

  mint : COMPONENT
  mint = mkComponent "Message Integrity"

  secWithAuth : SYSTEM
  secWithAuth = mkSystem "Secrecy with Authentication"

  secWithSig : SYSTEM
  secWithSig = mkSystem "Secrecy with Signature"

  sigWithApp : SYSTEM
  sigWithApp = mkSystem "Signature with Appendix"

  secWithInt : SYSTEM
  secWithInt = mkSystem "Secrecy with Integrity"

  secWithSigWithApp : SYSTEM
  secWithSigWithApp = mkSystem "Secrecy with Signature with Appendix"

ps : List (COMPONENT)
ps = [infosec, mauth, sauth, mint]

ps' : List (SYSTEM)
ps' = [secWithAuth, secWithSig, sigWithApp, secWithInt, secWithSigWithApp]

tropyc : GModel
tropyc = (insertMany ps' (insertMany ps (insertMany aims emptyModel)))
  \= mpatt
  \= (infosec ==< mpatt)
  \= (mint    ==< mpatt)
  \= (mauth   ==< mpatt)
  \= (sauth   ==< mpatt)

  \= (infosec o=> confidentiality      | MAKES)
  \= (mint    o=> msgIntegrity         | MAKES)
  \= (mauth   o=> subjectAuthenticity  | MAKES)
  \= (sauth   o=> originNonRepudiation | MAKES)

  \= (secWithAuth ]=>* [mauth, infosec])
-}
-- --------------------------------------------------------------------- [ EOF ]
