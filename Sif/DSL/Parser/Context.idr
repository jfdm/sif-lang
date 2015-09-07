-- ------------------------------------------------------------- [ Context.idr ]
-- Module    : Context.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Parser.Context

import Lightyear
import Lightyear.Strings

import Sif.Types

public
context : Parser $ Pair String SifDomain
context = do
    keyword "sif"
    string "context"
    eol
    space
    keyword "Context"
    t <- title
    keyword "as"
    i <- ident
    space
    d <- opt desc
    pure $ (i, MkDomain t d)
  <?> "Problem Specification"


-- --------------------------------------------------------------------- [ EOF ]
