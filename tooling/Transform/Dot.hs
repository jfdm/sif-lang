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
                    ps = Model.patterns plang

-- | Do the transformation of a (Local Patterns, Imports Patterns) tuple into Dot Form
doPatterns :: (Patterns, Patterns) -> [String]
doPatterns (pats, imps) = nub $ imports2Dot imps ++ patterns2Dot pats
                          
-- -- ---------------------------------------------------------- [ Imports to Dot ]

-- | Transform a list of Imported Patterns into Dot Form.
imports2Dot :: Patterns -> [String]
imports2Dot [] = [""]
imports2Dot imps = map (unlines . imports2Dot') getGroups
                   where
                     getGroups = groupBy groupImports imps
                     groupImports x y = Model.origin x == Model.origin y

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
patterns2Dot = map pattern2Dot

-- | Transform a Pattern into Dot Form
pattern2Dot :: Pattern -> String
pattern2Dot p = genDotNode (Model.ident p) (Model.name p) (Model.modifier p)

-- -------------------------------------------------------- [ Relations to Dot ]

-- | Transform the relations in the pattern language to Dot Form
doRelations :: Patterns -> [String]
doRelations = concatMap patternRels2Dot

                   
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
relations2Dot p (Just rs) t = map (\r -> genDotEdge (Model.ident p) (Model.to r) (Model.desc r) t) rs

-- -------------------------------------------------- [ Generate Dot Functions ]
-- | Gen Dot Node
genDotNode :: ID -> String -> Maybe Modifier -> String
genDotNode id label m = id ++ genDotNodeStyling label m

-- | Gen Dot Edge
genDotEdge :: ID -> ID -> Maybe String -> String -> String
genDotEdge a b l t =  a ++ " -> " ++ b ++ genDotEdgeStyling l t

-- | Generate Dot Edge Styling
genDotEdgeStyling :: Maybe String -> String -> String
genDotEdgeStyling desc t = " [" ++ styling ++ label ++ "];"
                           where
                             styling = case t of
                                         "implements" -> implements
                                         "extends" -> extends
                                         "uses" -> uses
                                         otherwise -> links

                             implements = "style=\"dotted\", dir=\"back\", arrowtail=\"empty\", "
                             extends = "style=\"solid\", dir=\"forward\", arrowhead=\"empty\", "
                             uses = "style=\"solid\", dir=\"back\", arrowtail=\"diamond\", "
                             links = "style=\"solid\", dir=\"forward\", arrowhead=\"normal\", "
                             label = if isNothing desc
                                     then ""
                                     else ", label=\"" ++ fromJust desc ++ "\""

-- | Generate Dot Node Styling
genDotNodeStyling :: String -> Maybe Modifier -> String
genDotNodeStyling l m = " [" ++ style ++ shape ++ label ++ "];"
                        where
                          shape = "shape=box, "
                          label = "label=\"" ++ l ++ "\""
                          style = if isNothing m
                                  then "style=rounded, " 
                                  else case fromJust m of
                                         Abstract -> "style=\"rounded,dashed\", "
                                         Integration -> "style=\"rounded,diagonals\", "
-- --------------------------------------------------------------------- [ EOF ]
