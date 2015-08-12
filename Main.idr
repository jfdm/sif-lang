module Main

import System
import Sif.Main

syntax MkSifEnv = with Env [ default,default,default,default
                             , default,default,default,default, default]

main : IO ()
main = do
  runInit (MkSifEnv) $ sifMain Nil
  exit 0
