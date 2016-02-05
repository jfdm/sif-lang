-- -------------------------------------------------------------- [ FileIO.idr ]
-- Module    : FileIO.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.FileIO

import Sif.Effs

import Sif.Error

-- TODO Make more robust by returning Maybe and Either
%access export

namespace Sif
  writeFile : String -> String -> Sif ()
  writeFile fname content = do
      trace $ unwords ["Writing file:", show fname]
      res <- File.writeFile (\x => CannotWriteFile x) fname content
      case res of
          Left  err => Sif.raise err
          Right _   => trace $ unwords ["Finished writing file,", show fname]

-- --------------------------------------------------------------------- [ EOF ]
