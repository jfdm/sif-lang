module Transform (plang2Dot) where

-- import Data.Graph.Inductive.Graph
-- import Text.Dot
import Data.Maybe
import Data.List
import Model.AST as AST

-- http://ivanmiljenovic.wordpress.com/2011/10/16/graphviz-in-vacuum/

-- ------------------------------------------------- [ Tranformation Functions ]
plang2Dot :: Plang -> [String]
plang2Dot plang | isNothing (AST.imports plang) = plang2Dot' plang False
                | otherwise = plang2Dot' plang True

plang2Dot' :: Plang -> Bool -> [String]
plang2Dot' plang b | b == False = heed ++ pbody ++ ["\n}\n"]
                   | otherwise = heed ++ ibody ++ pbody ++ ["\n}\n"]
                                 where
                                   heed = ["digraph G {\n"]
                                   pbody = doTransform plang
                                   ibody = imports2Dot $ fromJust (AST.imports plang)

doTransform :: Plang -> [String]
doTransform plang = patterns ++ classrels ++ instrels
                    where
                      patterns = patterns2Dot plang
                      classrels = classRelations2Dot plang
                      instrels = instRelations2Dot plang

-- ---------------------------------------------------------- [ Imports to Dot ]

imports2Dot :: Imports -> [String]
imports2Dot imps = map unlines (map (imports2Dot') (groupBy (groupImports) imps))
    where
      groupImports x y = (AST.lang x) == (AST.lang y)
     
     
imports2Dot' :: Imports -> [String]
imports2Dot' imps = heed ++ body ++ taal
                    where
                      heed = ["subgraph cluster_" ++ label ++ " {\n"]
                      body = map (import2Dot) imps
                      taal = ["label=\"" ++ label ++ "\"\ncolor=black;\n}\n"]
                      label = (AST.lang (head imps))

import2Dot :: Import -> String
import2Dot (Import id Nothing) = genDotNode id id 
import2Dot (Import id (Just patID)) = genDotNode patID patID

-- @TODO If full import -> 
--    1. Get AST for import language.
--    2. Transform into Plang
 
--      withImports = ["cluster_" ++ (AST.label (AST.info plang)) ++ "{\n"]

-- ------------------------------------------------- [ General Relations 2 Dot ]

instRelations2Dot :: Plang -> [String]
instRelations2Dot plang = concat $ map (\x -> instRelation2Dot x ) (AST.relations plang)

instRelation2Dot :: Relation -> [String]
instRelation2Dot rel = map (\to -> genDotEdge from to desc)  (AST.to rel)
                       where
                         from = (AST.from rel)
                         desc = (AST.desc rel)

-- --------------------------------------------------- [ Class Relations 2 Dot ]
classRelations2Dot :: Plang -> [String]
classRelations2Dot plang = union extends implements
                           where
                             extends = concat $ mapMaybe (extends2Dot) (AST.patterns plang)
                             implements = concat $ mapMaybe (implements2Dot) (AST.patterns plang)
                             
-- | Get Implements
implements2Dot :: Pattern -> Maybe [String]
implements2Dot (Pattern _ _ _ Nothing _) = Nothing
implements2Dot (Pattern _ i _ (Just xs) _) = Just $ map (\x -> genDotEdge i x (Just "implements")) xs

-- | Get Extends to Dot
extends2Dot :: Pattern -> Maybe [String]
extends2Dot (Pattern _ _ Nothing _ _) = Nothing
extends2Dot (Pattern _ i (Just xs) _ _) = Just $ map (\x -> genDotEdge i x (Just "extends")) xs

-- ---------------------------------------------------------- [ Patterns 2 Dot ]

-- | Patterns to Dot
patterns2Dot :: Plang -> [String]
patterns2Dot plang = map pattern2Dot (AST.patterns plang)

-- | Pattern to Dot
pattern2Dot :: Pattern -> String
pattern2Dot p = genDotNode (AST.ident p) (AST.name p)

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
