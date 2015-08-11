module Main

import System
import Sif.Main


main : IO ()
main = do
  run (sifMain Nil)
  exit 0
