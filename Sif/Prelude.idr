-- ------------------------------------------------------------- [ Pattern.idr ]
-- Module    : Pattern.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Prelude

import Sif.Pattern
import Sif.Library

import Sif.Prelude.Problem
import Sif.Prelude.Solution
import Sif.Prelude.Pattern


library : SifLib
library = addToLibraryM
    [referenceMonitor,infoSecAsymCrypto,infoSecSymmCrypto]
    emptyLib

-- --------------------------------------------------------------------- [ EOF ]
