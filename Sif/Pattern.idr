-- ------------------------------------------------------------- [ Pattern.idr ]
-- Module    : Pattern.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Pattern

import public GRL.Lang.GLang

import Sif.Types

import public Sif.Pattern.Model
import public Sif.Pattern.API
import public Sif.Pattern.Convert

%default partial
%access public

-- --------------------------------------------------- [ Public Data Structure ]

instance Show (PATTERN impl d) where
  show x =
    case showConvPattern STRING x of
      Nothing => show $ "Internal Err"
      Just r  => r

-- --------------------------------------------------------------------- [ EOF ]
