module Sif.Examples.Tropyc

import Sif.PLangSpec

using (G : List LTy)

  tropyc : Expr G LANG
  tropyc = with List (sif (do
    let metapattern = Component "Cryptographic MetaPattern"
    let mauth = Component "Message Authentication"
    let infosec = Component "Information Secrecy"
    let senderauth = Component "Sender Authentication"
    let msgintegrity = Component "Message Integrity"

    let swa = System "Secrecy with Authentication"
    let sws = System "Secrecy with Signature"
    let swapp = System "Signature with Appendix"
    let swi = System "Secrecy with Integrity"

    let swswa = System "Secrecy with Signature with Appendix"

    MkLang Nil
         [MkNode metapattern,
          MkNode mauth,
          MkNode infosec,
          MkNode infosec,
          MkNode infosec,
          MkNode senderauth,
          MkNode msgintegrity,
          MkNode swa,
          MkNode sws,
          MkNode swapp,
          MkNode swswa]
         Nil
         [Specialises mauth metapattern,
          Specialises infosec metapattern,
          Specialises senderauth metapattern,
          Specialises msgintegrity metapattern,
          Uses swa mauth,
          Uses swa infosec,
          Uses sws infosec,
          Uses sws senderauth,
          Uses swapp senderauth,
          Uses swapp msgintegrity,
          Uses swi infosec,
          Uses swi msgintegrity,
          Uses swswa sws,
          Uses swswa swapp
          ]))

-- main : IO ()
-- main = print $ run $ compile tropyc
