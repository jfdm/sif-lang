module Transform.Dot (plang2Dot) where

import Data.Maybe
import Data.List
import Model
import Examples.Tampering
import Examples.Tropyc

-- ------------------------------------------------- [ Tranformation Functions ]
-- | Transform a Pattern Language into it's Dot equivalent
plang2Dot :: Plang -> [String]
plang2Dot plang = heed ++ pbody ++ rbody ++ foot
                  where
                    heed = ["digraph G {\n"]
                    foot = ["\n}\n"]
                    pbody = doPatterns $ break (isJust . Model.origin) ps
                    rbody = doRelations ps
                    ps = (Model.patterns plang)

-- | Do the transformation of a (Local Patterns, Imports Patterns) tuple into Dot Form
doPatterns :: (Patterns, Patterns) -> [String]
doPatterns (pats, imps) = nub $ imports2Dot imps ++ patterns2Dot pats
                          
-- -- ---------------------------------------------------------- [ Imports to Dot ]

-- | Transform a list of Imported Patterns into Dot Form.
imports2Dot :: Patterns -> [String]
imports2Dot [] = [""]
imports2Dot imps = map unlines (map imports2Dot' getGroups)
                   where
                     getGroups = groupBy groupImports imps
                     groupImports x y = (Model.origin x) == (Model.origin y)

-- | Do the transformation of Imports
imports2Dot' :: Patterns -> [String]
imports2Dot' imps = heed ++ body ++ foot
                    where
                      heed = ["subgraph cluster_" ++ label ++ " {\n"]
                      foot = ["label=\"" ++ label ++ "\"\ncolor=black;\n}\n"]
                      label = fromJust (Model.origin (head imps))
                      body = patterns2Dot imps

-- ---------------------------------------------------------- [ Pattern To Dot ]

-- | Transform a List of Patterns to Dot Form.

patterns2Dot :: Patterns -> [String]
patterns2Dot ps = map pattern2Dot ps

-- | Transform a Pattern into Dot Form
pattern2Dot :: Pattern -> String
pattern2Dot p = genDotNode (Model.ident p) (Model.name p)

-- -------------------------------------------------------- [ Relations to Dot ]

-- | Transform the relations in the pattern language to Dot Form
doRelations :: Patterns -> [String]
doRelations ps = concatMap patternRels2Dot ps

                   
patternRels2Dot :: Pattern -> [String]
patternRels2Dot p = nub $ extends2Dot ++ implements2Dot ++ requires2Dot ++ generals2Dot
                    where 
                      extends2Dot    = relations2Dot p (Model.extends p)    "extends"
                      implements2Dot = relations2Dot p (Model.implements p) "implements"
                      requires2Dot   = relations2Dot p (Model.requires p)   "uses"
                      generals2Dot   = relations2Dot p (Model.links p)      "links"

-- | Generic Transform of Relation to Dot Form
relations2Dot :: Pattern -> Maybe Relations -> String -> [String]
relations2Dot _ Nothing _ = [""]
relations2Dot p (Just rs) desc = map (\r -> genDotEdge (Model.ident p) (Model.to r) (chkDesc r)) rs
  where
    chkDesc r = case isNothing $ Model.desc r of True -> Just desc
                                                 otherwise -> Model.desc r

-- -------------------------------------------------- [ Generate Dot Functions ]
-- | Gen Dot Node
genDotNode :: ID -> String -> String
genDotNode i l = i ++ " [style=rounded,shape=box,label=\"" ++ l ++ "\"]; "

-- | Gen Dot Edge
genDotEdge :: ID -> ID -> Maybe String -> String
genDotEdge a b Nothing = a ++ " -> " ++ b ++ ";"
genDotEdge a b (Just l) =  a ++ " -> " ++ b ++ " [" ++ label l ++ "]; "
                           where
                             label l = "label=\"" ++ l ++ "\""

-- --------------------------------------------------------------------- [ EOF ]
