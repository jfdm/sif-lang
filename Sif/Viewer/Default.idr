-- ------------------------------------------------------------- [ Default.idr ]
-- Module    : Default.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

||| Sample and Default Viewer for the built in Pattern Library.
module Sif.Viewer.Default

import Sif.Viewer
import Sif.Pattern

import Sif.Lib

namespace Main
  main : IO ()
  main = do
    run (modelViewer library)
    exit 0

-- --------------------------------------------------------------------- [ EOF ]
