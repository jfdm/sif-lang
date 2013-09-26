-- | A pretty printing function for the AST
module Pretty.AST
    (
     prettyPlangAST
    ) where

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
                       <$$> prettyImports imps
                       <$$> empty
                       <$$> string sifKWordPattern
                       <$$> empty
                       <$$> prettyPatterns patts
                       <$$> empty
                       <$$> string sifKWordRelation
                       <$$> empty
                       <$$> prettyRelations (relations plang)
                       where
                         (patts, imps) = break (isJust . origin) (patterns plang)

-- ---------------------------------------------------- [ Language Declaration ]
-- | Prettify Language Declaration
prettyMetadata :: String -> ID -> Doc
prettyMetadata title id = string sifKWordLang
                          <+> dquotes (string title)
                          <+> string sifKWordAs
                          <+> string id

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
                     f p = string sifKWordFrom
                           <+> string (fromJust (origin p))
                           <+> string sifKWordImport
                           <+> string (ident p)

-- | Prettify a Language Import
prettyLangImport :: PatternsExpr -> Doc
prettyLangImport ps = vsep $ map f ps
                      where
                        f p = string sifKWordImport <+> string (fromJust (origin p))


-- ---------------------------------------------------------------- [ Patterns ]

-- | Prettify declared local patterns
prettyPatterns :: PatternsExpr -> Doc
prettyPatterns [] = empty
prettyPatterns ps = vsep $ map prettyPattern ps

-- | Prettify a Pattern
prettyPattern :: PatternExpr -> Doc
prettyPattern p = text (ident p)
                  <+> string sifOpAssignment
                  <+> string mod
                  <+> string ptype
                  <+> string sifKWordTypPat
                  <> parens (dquotes (string (fromMaybe "" (name p))))
                  where
                    mod = case modifier p of
                            TyModAbstract -> sifKWordTypModAbs
                            TyModConcrete -> sifKWordTypModConc
                    ptype = case typ p of 
                              TyComponent      -> sifKWordTypComp
                              TyPattern        -> sifKWordTypPat
                              TySystem         -> sifKWordTypSys
                              TyDeployment     -> sifKWordTypDeplo
                              TyAdmin          -> sifKWordTypAdmin
                              TyImplementation -> sifKWordTypImpl

-- --------------------------------------------------------------- [ Relations ]

-- | Prettify the relations of the Patterns
prettyRelations :: RelationsExpr -> Doc
prettyRelations [] = empty
prettyRelations rs = vsep $ map prettyRelation (reverse rs)

-- | Prettify a list of relations
prettyRelation :: RelationExpr -> Doc
prettyRelation rel = string (from rel)
                     <+> string rtyp
                     <+> string (to rel)
                     <+> descrip
                     where
                       descrip = if isNothing (desc rel)
                                 then empty
                                 else colon <+> string (fromJust (desc rel))
                       rtyp = case rtype rel of
                                 TyAssociation    -> sifOpAssociation
                                 TySpecialisation -> sifOpSpecialisation
                                 TyRealisation    -> sifOpRealisation
                                 TyAggregation    -> sifOpAggregation

-- --------------------------------------------------------------------- [ EOF ]
