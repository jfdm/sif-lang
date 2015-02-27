module Sif.Examples.Tropyc

import Sif.LangShallow

using (G : List LTy)

  infosec : Expr G $ PATTERN DEPLOY
  infosec = sif (do
    let f1 = Functional  "1 lah"
    let f2 = Performance "2 lah"
    let f3 = Reliability "3 lah"
    let f4 = Performance "4 lah"
    let f5 = Usability   "5 lah"
    Deploy [f1,f2,f3,f4,f5] Nil)

  infosec' : Expr G $ PATTERN GENERIC
  infosec' = sif (do
    let f1 = Functional  "1 blah"
    let f2 = Performance "2 blah"
    let f3 = Reliability "3 blah"
    let f4 = Performance "4 blah"
    let f5 = Usability   "5 blah"
    Generic [f1,f2,f3,f4,f5] Nil)

  tropyc : Expr G LANG
  tropyc = sif (do
    let x = infosec
    let y = infosec'
    Lang [MkPattern x, MkPattern y] [LinkedTo x y])

-- main : IO ()
-- main = print $ run $ compile tropyc
