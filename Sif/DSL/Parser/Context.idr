-- ------------------------------------------------------------- [ Context.idr ]
-- Module    : Context.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser.Context

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Sif.Types
import Sif.DSL.Parser.Utils
import Sif.DSL.Parser.Common

export
context : Parser $ Pair String SifDomain
context = do
    keyword "sif"
    string "context"
    endOfLine
    spaces
    keyword "Context"
    t <- title
    keyword "as"
    i <- ident
    spaces
    d <- opt desc
    pure $ (i, MkDomain t d)
  <?> "Problem Specification"


-- --------------------------------------------------------------------- [ EOF ]
