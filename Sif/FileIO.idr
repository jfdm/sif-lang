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

readFile : String -> Eff String SifEffs
readFile fname = do
    trace $ unwords ["Reading file:", show fname]
    case !(open fname Read) of
      True => do
        src <- readAcc ""
        close
        pure src
      False => Sif.raise (FileMissing fname)
  where
    readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
    readAcc acc = if (not !(eof))
                     then readAcc (acc ++ !(readLine))
                     else pure acc

-- --------------------------------------------------------------------- [ EOF ]
