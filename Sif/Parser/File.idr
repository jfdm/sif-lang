-- ---------------------------------------------------------------- [ File.idr ]
-- Module    : File.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser.File

import Lightyear
import Lightyear.Strings

import Effects
import Effect.Exception
import Effect.File

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
           -> Eff a [FILE_IO (), EXCEPTION String]
readSifFile p f = do
    case !(open f Read) of
      True => do
        src <- readFile
        close
        case parse p src of
          Left err  => raise err
          Right res => pure res
      False => raise $ "Cannot Read File Error: " ++ f

-- --------------------------------------------------------------------- [ EOF ]
