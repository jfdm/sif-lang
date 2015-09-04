-- ------------------------------------------------------------- [ Pattern.idr ]
-- Module    : Pattern.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Prelude

import public Config.Error
import public Config.YAML

%access public


-- ------------------------------------------------------------ [ YAML Parsing ]

{-
Schema is

file ::= pattern+
pattern ::= "pattern": {"problem":!!str, "solution": (vale::!!str)}

-}

private
getString : YAMLNode -> Maybe String
getString (YAMLString s) = Just s
getString _              = Nothing

private
isPatternMap : YAMLNode -> Bool
isPatternMap (YAMLString str) = toLower str == "pattern"
isPatternMap _ = False

private
getPatternKVPairs : YAMLNode -> List (YAMLNode)
getPatternKVPairs (YAMLDoc _ doc) with (doc)
  | (YAMLMap ps) = map snd $ filter (\(x,y) => isPatternMap x) ps
  | otherwise = Nil
getPatternKVPairs _ = Nil

private
getStringKey : (YAMLNode, YAMLNode) -> Maybe String
getStringKey (k,_) = getString k

private
getStringValue : (YAMLNode, YAMLNode) -> Maybe String
getStringValue (_,v) = getString v

private
getPSPair : YAMLNode -> Maybe (String, String)
getPSPair (YAMLMap [p,s]) =
    case (getStringKey p, getStringKey s) of
      (Just p', Just s') =>
        if toLower p' == "problem" && toLower s' == "solution"
          then case (getStringValue p, getStringValue s) of
            (Just p'', Just s'') => Just (p'', s'')
            otherwise            => Nothing
          else Nothing
      otherwise          => Nothing
getPSPair _  = Nothing

filterPreludeIDX : YAMLNode -> List (String, String)
filterPreludeIDX doc = mapMaybe (getPSPair) $ getPatternKVPairs doc

-- --------------------------------------------------------------------- [ EOF ]
