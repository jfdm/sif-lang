-- | A pretty printing function for the AST
module Util.PrettyAST ( prettyPlangAST ) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe
import Data.List
import Data.Function

import AST
import Types
import Keywords

-- | A Pretty Printer of our plang AST.
prettyPlangAST :: PlangAST -> Doc
prettyPlangAST plang = prettyMetadata (title plang) (label plang) 
                       <$$> empty
                       <$$> prettyImports is
                       <$$> empty
                       <$$> text sifKWordPattern
                       <$$> empty
                       <$$> prettyPatterns ps
                       <$$> empty
                       <$$> text sifKWordRelation
                       <$$> empty
                       <$$> prettyRelations (relations plang)
                       where
                         (ps, is) = break (isJust . origin) (patterns plang)

-- ---------------------------------------------------- [ Language Declaration ]
-- | Prettify Language Declaration
prettyMetadata :: String -> ID -> Doc
prettyMetadata title id = text sifKWordLang
                          <+> dquotes (text title)
                          <+> text sifKWordAs
                          <+> text id

-- ----------------------------------------------------------------- [ Imports ]

-- | Prettify the imports
prettyImports :: PatternsExpr -> Doc
prettyImports [] = empty
prettyImports is = vsep $ map prettyImport domains
                   where 
                     domains = groupBy ((==) `on` origin) is

-- | Prettify a single import domain
prettyImport :: PatternsExpr -> Doc
prettyImport ps = prettyImportM imps <$> prettyLangImport langImps
                  where
                    (langImps, imps) = break (\x -> ident x /= fromJust (origin x)) ps

-- | Prettify individual pattern imports
prettyImportM :: PatternsExpr -> Doc
prettyImportM ps = vsep $ map f ps
                   where
                     f p = text sifKWordFrom
                           <+> text (fromJust (origin p))
                           <+> text sifKWordImport
                           <+> text (ident p)

-- | Prettify a Language Import
prettyLangImport :: PatternsExpr -> Doc
prettyLangImport ps = vsep $ map f ps
                      where
                        f p = text sifKWordImport <+> text (fromJust (origin p))


-- ---------------------------------------------------------------- [ Patterns ]

-- | Prettify declared local patterns
prettyPatterns :: PatternsExpr -> Doc
prettyPatterns [] = empty
prettyPatterns ps = vsep $ map prettyPattern ps

-- | Prettify a Pattern
prettyPattern :: PatternExpr -> Doc
prettyPattern p = text (ident p)
                  <+> text sifOpAssignment
                  <+> text mod
                  <+> t
                  <+> text sifKWordTypPat
                  <> parens (dquotes (text (fromMaybe "" (name p))))
                  where
                    mod = case modifier p of
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

-- | Prettify the relations of the Patterns
prettyRelations :: RelationsExpr -> Doc
prettyRelations [] = empty
prettyRelations rs = vsep $ map prettyRelation (reverse rs)

-- | Prettify a list of relations
prettyRelation :: RelationExpr -> Doc
prettyRelation r = text (from r)
                   <+> t
                   <+> text (to r)
                   <+> descrip
                   where
                     descrip = if isNothing (desc rel)
                               then empty
                               else colon <+> text (fromJust (desc rel))
                     t = case rtype rel of
                           TyAssociation    ->text sifOpAssociation
                           TySpecialisation ->text sifOpSpecialisation
                           TyRealisation    ->text sifOpRealisation
                           TyAggregation    ->text sifOpAggregation

-- --------------------------------------------------------------------- [ EOF ]
