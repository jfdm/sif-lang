-- | Pretty print the pattern language specification to the original
-- Sif input format.
-- 
-- Notably all lists and imports will be flattened.
module Sif.Transform.Sif ( plang2Sif, extSif ) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe
import Data.List
import Data.Function

import Sif.Model
import Sif.Types
import Sif.Keywords

-- | File extension
extSif = ".sif"

-- | Transform a Pattern Language into it's Sif equivalent
plang2Sif :: PlangSpec -> Doc
plang2Sif plang = prettyMetadata (title plang) (label plang)
                  <$$> empty
                  <$$> prettyImports is
                  <$$> empty
                  <$$> string sifKWordPattern
                  <$$> empty
                  <$$> prettyPatterns ps
                  <$$> empty
                  <$$> string sifKWordRelation
                  <$$> empty
                  <$$> prettyRelations (relations plang)
                  where                         
                    is = imports plang
                    ps = patterns plang

-- ---------------------------------------------------- [ Language Declaration ]
-- | Prettify Language Declaration
prettyMetadata :: String -> String -> Doc
prettyMetadata title id = string sifKWordLang
                          <+> dquotes (string title)
                          <+> string sifKWordAs
                          <+> string id

-- ----------------------------------------------------------------- [ Imports ]

-- | Prettify the imports
-- @TODO
prettyImports :: Patterns -> Doc
prettyImports ps = empty

prettyImport :: Pattern -> Doc
prettyImport = undefined

-- ---------------------------------------------------------------- [ Patterns ]
-- | Prettyify local patterns
prettyPatterns :: Patterns -> Doc
prettyPatterns ps = vsep $ map prettyPattern ps

-- | Prettify a Pattern
prettyPattern :: Pattern -> Doc
prettyPattern p = text id
                  <+> text sifOpAssignment
                  <+> text m
                  <+> t
                  <+> text sifKWordTypPat
                  <> parens (dquotes (string (fromMaybe "" (name p))))
                  where
                    id = ident p
                    m = case modifier p of 
                          TyModAbstract -> sifKWordTypModAbs
                          TyModConcrete -> sifKWordTypModConc
                    t = case ptype p of 
                          TyComponent      -> text sifKWordTypComp
                          TySystem         -> text sifKWordTypSys
                          TyDeployment     -> text sifKWordTypDeplo
                          TyAdmin          -> text sifKWordTypAdmin
                          TyImplementation -> text sifKWordTypImpl
                          TyPattern        -> empty

-- --------------------------------------------------------------- [ Relations ]

-- | Prettify Relations
prettyRelations :: Relations -> Doc
prettyRelations rs = vsep $ map prettyRelation rs

-- | Prettify Relation
prettyRelation :: Relation -> Doc
prettyRelation r = text ((ident . from) r)
                   <+> text rtyp
                   <+> text ((ident . to) r)
                   <+> descrip
                   where
                     descrip = if isNothing (desc r)
                               then empty
                               else colon <+> text (fromJust (desc r))
                     rtyp = case rtype r of
                              TyAssociation    -> sifOpAssociation
                              TySpecialisation -> sifOpSpecialisation
                              TyRealisation    -> sifOpRealisation
                              TyAggregation    -> sifOpAggregation

-- --------------------------------------------------------------------- [ EOF ]
