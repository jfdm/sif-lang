-- | Modules to transform the Pattern Language into various output formats.
module Sif.Transform where

import Data.Char
import Text.PrettyPrint.Leijen as PP

import Sif.Model
import Sif.Types

import Sif.Transform.Dot
import Sif.Transform.Sif

-- | Try and transform the pattern language into the specified format.
-- If a supported format return a tuple containing the desired file
-- extension, and the transformed specification.
transformPlang :: String
               -> PlangSpec
               -> Either String (String, Doc)
transformPlang fmt plang
    | fmt == fmtDot = Right (extDot, plang2Dot plang)
    | fmt == fmtSif = Right (extSif, plang2Sif plang)
    | otherwise     = Left $ "Unsupported Format: " ++ fmt


-- --------------------------------------------------------------------- [ EOF ]
