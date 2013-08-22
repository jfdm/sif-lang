module Transform (plang2Dot) where

-- import Data.Graph.Inductive.Graph
-- import Text.Dot
import Data.Maybe
import Data.List
import Model
import Examples 

-- http://ivanmiljenovic.wordpress.com/2011/10/16/graphviz-in-vacuum/

-- ------------------------------------------------- [ Tranformation Functions ]

plang2Dot :: Plang -> [String]
plang2Dot plang = ["digraph G {\n"] ++ patterns ++ classrels ++ instrels ++ ["\n}\n"]
    where
      patterns = patterns2Dot plang
      classrels = classRelations2Dot plang
      instrels = instRelations2Dot plang
-- ------------------------------------------------- [ General Relations 2 Dot ]

instRelations2Dot :: Plang -> [String]
instRelations2Dot plang = concat $ map (\x -> instRelation2Dot x ) (Model.relations plang)

instRelation2Dot :: Relation -> [String]
instRelation2Dot rel = map (\to -> genDotEdge from to desc)  (Model.to rel)
                       where
                         from = (Model.from rel)
                         desc = (Model.desc rel)

-- --------------------------------------------------- [ Class Relations 2 Dot ]
classRelations2Dot :: Plang -> [String]
classRelations2Dot plang = union extends implements
                           where
                             extends = concat $ mapMaybe (extends2Dot) (Model.patterns plang)
                             implements = concat $ mapMaybe (implements2Dot) (Model.patterns plang)
                             

-- | Get Implements
implements2Dot :: Pattern -> Maybe [String]
implements2Dot (Pattern _ _ _ Nothing _) = Nothing
implements2Dot (Pattern _ i _ (Just xs) _) = Just $ map (\x -> genDotEdge x i (Just "implements")) xs

-- | Get Extends to Dot
extends2Dot :: Pattern -> Maybe [String]
extends2Dot (Pattern _ _ Nothing _ _) = Nothing
extends2Dot (Pattern _ i (Just xs) _ _) = Just $ map (\x -> genDotEdge i x (Just "extends")) xs

-- ---------------------------------------------------------- [ Patterns 2 Dot ]

-- | Patterns to Dot
patterns2Dot :: Plang -> [String]
patterns2Dot plang = map pattern2Dot (Model.patterns plang)

-- | Pattern to Dot
pattern2Dot :: Pattern -> String
pattern2Dot p = genDotNode (Model.ident p) (Model.name p)

-- -------------------------------------------------- [ Generate Dot Functions ]
-- | Gen Dot Node
genDotNode :: ID -> String -> String
genDotNode i l = i ++ " [style=rounded,shape=box,label=\"" ++ l ++ "\"]; "

-- | Gen Dot Edge
genDotEdge :: ID -> ID -> Maybe String -> String
genDotEdge a b Nothing = a ++ " -> " ++ b ++ ";"
genDotEdge a b (Just l) =  a ++ " -> " ++ b ++ "[label=\"" ++ l ++ "\"]; "

