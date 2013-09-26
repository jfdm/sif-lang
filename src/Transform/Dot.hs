-- | Transform the Pattern Language into a Dot representation.
module Transform.Dot (plang2Dot, extDot) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe
import Data.List
import Data.Function

import Model
import Types
import Keywords

-- | File extension
extDot = ".dot"

-- | Transform a Pattern Language into it's Dot equivalent
plang2Dot :: PlangSpec -> Doc
plang2Dot plang = text "digraph" <+> text "G" <+> lbrace
                  <$$> indent 4 (vcat [ibody, pbody, rbody])
                  <$$> rbrace
                  where
                    ibody = imports2Dot (imports plang)     <$$> empty
                    pbody = patterns2Dot (patterns plang)   <$$> empty
                    rbody = relations2Dot (relations plang) 

-- ----------------------------------------------------------------- [ Imports ]

-- | Dottify the Imports
-- @TODO
imports2Dot :: Patterns -> Doc
imports2Dot is = empty

-- | Dottify the Import
import2Dot :: Pattern -> Doc
import2Dot i = empty

-- ---------------------------------------------------------------- [ Patterns ]

-- | Dottify the Patterns
patterns2Dot :: Patterns -> Doc
patterns2Dot ps = vsep $ map pattern2Dot ps

-- | Dottify the pattern.
pattern2Dot :: PatternItem -> Doc
pattern2Dot (k, v) = text k <+>
                     genPatternStyle v k <>
                     semi

-- | Generate the pattern style
genPatternStyle :: Pattern -> String -> Doc
genPatternStyle p id = genStyle [genPatternShape (ptype p),       -- Shape 
                                 genModifierStyle (modifier p),   -- Line
                                 genLabel (fromMaybe id (name p)) -- Label 
                                ]

-- | Generate the Pattern Shapes.
--
-- The shapes were chosen at random.
genPatternShape :: TyGenPattern -> Doc
genPatternShape t = case t of 
                      TyComponent      -> genStylePair "shape"  "septagon"
                      TySystem         -> genStylePair "shape"  "octagon"
                      TyDeployment     -> genStylePair "shape"  "doubleoctagon"
                      TyAdmin          -> genStylePair "shape"  "house"
                      TyImplementation -> genStylePair "shape"  "trapezium"
                      TyPattern        -> genStylePair "shape"  "ellipse"

-- | Determine the modifier styles.
genModifierStyle :: TyModifier -> Doc
genModifierStyle m = case m of
                       TyModAbstract -> genStyle' "dashed"
                       TyModConcrete -> genStyle' "solid"
                     where
                       genStyle' style = genStylePair "style" style

-- --------------------------------------------------------------- [ Relations ]

-- | Dottify the relations
relations2Dot :: Relations -> Doc
relations2Dot rs = vsep $ map relation2Dot rs

relation2Dot :: Relation -> Doc
relation2Dot r = text (from r)
                 <+> text "->"
                 <+> text (to r)
                 <+> genRelationStyle (rtype r) (desc r)
                 <> semi

genRelationStyle :: TyRelation -> Maybe String -> Doc
genRelationStyle t d = genStyle [genStylePair "style" lStyle,  -- Line
                                 genStylePair "dir" dStyle,    -- Direction
                                 genStylePair arrowEnd aStyle, -- Arrow
                                 maybe empty genLabel d        -- Label
                                ]
                       where
                         (lStyle, dStyle, arrowEnd, aStyle) = genArrowStyle t
                         

genArrowStyle :: TyRelation -> (String, String, String, String)
genArrowStyle t = case t of
                    TySpecialisation -> ("solid", "back", "arrowtail", "onormal")
                    TyAggregation    -> ("solid", "back", "arrowtail", "odiamond")
                    TyRealisation    -> ("dashed", "back", "arrowtail", "empty")
                    TyAssociation    -> ("solid", "forward", "arrowhead", "normal")


-- -------------------------------------------------------------------- [ Util ] 
-- | Generate the syntactic sugar for styles in Dot.
genStyle :: [Doc] -> Doc
genStyle styling = brackets (hcat (punctuate comma styling))

-- | Generate a Dot <key,value> pairing.
genStylePair :: String -> String -> Doc
genStylePair k v = text k <> equals <> dquotes (text v)

genLabel :: String -> Doc
genLabel l = genStylePair "label" l

-- --------------------------------------------------------------------- [ EOF ]
