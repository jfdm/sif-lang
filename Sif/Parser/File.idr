-- ---------------------------------------------------------------- [ File.idr ]
-- Module    : File.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser.File

import Lightyear
import Lightyear.Strings

import Sif.Error
import Sif.Effs

%default partial

readFile : Eff String [FILE_IO (OpenFile Read)]
readFile = readAcc ""
  where
    readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
    readAcc acc = if (not !eof)
                     then readAcc (acc ++ !readLine)
                     else pure acc

public
readSifFile : Parser a
           -> String
           -> Eff a [FILE_IO (), 'sif ::: EXCEPTION SifError]
readSifFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => Sif.raise (ParseError err)
          Right res => pure res
      False => Sif.raise (FileMissing f)

-- --------------------------------------------------------------------- [ EOF ]
