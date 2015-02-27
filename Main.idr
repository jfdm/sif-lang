module Main

import Sif.Model
import Sif.Pattern
import Sif.Interpret
import Sif.Evaluate

import Sif.Examples.Tropyc

main : IO ()
main = do
  m <- run $ interp (sif infosec)
  res <- run (evalPattern m)
  putStrLn $ show res
