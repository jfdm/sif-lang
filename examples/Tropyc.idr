||| Modelling of the Tropyc Pattern Language.
module Tropyc

import Sif.PLangSpec

tropyc : Stmt g
tropyc = with List (sif (do
  let metapattern = Generic "Cryptographic MetaPattern"
  let mauth = Component "Message Authentication"
  let infosec = Component "Information Secrecy"
  let senderauth = Component "Sender Authentication"
  let msgintegrity = Component "Message Integrity"

  let swa = System "Secrecy with Authentication"
  let sws = System "Secrecy with Signature"
  let swapp = System "Signature with Appendix"
  let swi = System "Secrecy with Integrity"

  let swswa = System "Secrecy with Signature with Appendix"

  Dcl $ Specialises mauth metapattern
  Dcl $ Specialises infosec metapattern
  Dcl $ Specialises senderauth metapattern
  Dcl $ Specialises msgintegrity metapattern
  Dcl $ Uses swa mauth
  Dcl $ Uses swa infosec
  Dcl $ Uses sws infosec
  Dcl $ Uses sws senderauth
  Dcl $ Uses swapp senderauth
  Dcl $ Uses swapp msgintegrity
  Dcl $ Uses swi infosec
  Dcl $ Uses swi msgintegrity
  Dcl $ Uses swswa sws
  Dcl $ Uses swswa swapp)

-- main : IO ()
-- main = print $ run $ compile tropyc
