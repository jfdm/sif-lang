-- ------------------------------------------------------------- [ Default.idr ]
-- Module    : Default.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Sample and Default Viewer for the built in Pattern Library.
module Sif.Viewer.Default

import System
import Sif.Pattern
import Sif.API
import Sif.Viewer.REPL


namespace Main
  main : IO ()
  main = do
    run (modelViewer emptyLib)
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
