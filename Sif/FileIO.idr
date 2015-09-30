-- -------------------------------------------------------------- [ FileIO.idr ]
-- Module    : FileIO.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.FileIO

import Sif.Effs
import Sif.Error

-- TODO Make more robust by returning Maybe and Either

writeFile : String -> String -> Eff () SifEffs
writeFile fname content = do
    trace $ unwords ["Writing file:", show fname]
    case !(open fname Write) of
      True => do
        writeString content
        close
        trace $ unwords ["Finished writing file,", show fname]
      False => Sif.raise (CannotWriteFile fname)


-- --------------------------------------------------------------------- [ EOF ]
