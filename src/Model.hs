-- | Model for our Pattern Language
module Model where

import TypeSystem

data PLangSpec = PlangSpec {
      title    :: String,
      label    :: ID,
      patterns :: PatternsExpr,
      relations :: RelationsExpr
    } deriving (Show)
