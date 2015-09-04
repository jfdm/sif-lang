-- ------------------------------------------------------------- [ Pattern.idr ]
-- Module    : Pattern.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern

import public GRL.Lang.GLang

import Sif.Types
import Sif.Error

import public Sif.Pattern.Model
import public Sif.Pattern.API
import public Sif.Pattern.Convert

%default partial
%access public

-- --------------------------------------------------- [ Public Data Structure ]

instance Show (PATTERN impl) where
  show x =
    case showConvPattern STRING x of
      Nothing => show $ InternalErr
      Just r  => r

-- --------------------------------------------------------------------- [ EOF ]
