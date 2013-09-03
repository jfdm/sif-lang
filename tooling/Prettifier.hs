module Prettifier (prettyPlang) where

import Text.PrettyPrint.Leijen as PP
import Data.Maybe
import Data.List
import Data.Function
import Model
import Utils

import Examples.Tampering

-- | A Pretty Printer of our plang AST.
prettyPlang :: Plang -> Doc
prettyPlang plang = prettyMetadata (Model.title plang) (Model.label plang) 
                    <$$> empty
                    <$$> prettyImports imps
                    <$$> empty
                    <$$> string "patterns"
                    <$$> prettyPatterns patts
                    <$$> empty
                    <$$> string "relations"
                    <$$> prettyRelations (Model.patterns plang)
                    where
                      (patts, imps) = break (isJust . Model.origin) (Model.patterns plang)

-- ---------------------------------------------------- [ Language Declaration ]
-- | Prettify Language Declaration
prettyMetadata :: String -> ID -> Doc
prettyMetadata title id = string "language"
                          <+> dquotes (string title)
                          <+> string "as"
                          <+> string id

-- ----------------------------------------------------------------- [ Imports ]

-- | Prettify the imports
prettyImports :: Patterns -> Doc
prettyImports [] = empty
prettyImports is = vsep $ map prettyImport domains
                   where 
                     domains = groupBy ((==) `on` Model.origin) is

-- | Prettify a single import domain
prettyImport :: Patterns -> Doc
prettyImport ps = prettyImportM imps <$> prettyLangImport langImps
                  where
                    (langImps, imps) = break (\x -> Model.ident x /= fromJust (Model.origin x)) ps

-- | Prettify individual pattern imports
prettyImportM :: Patterns -> Doc
prettyImportM ps = vsep $ map f ps
                   where
                     f p = string "from" 
                           <+> string (fromJust (Model.origin p))
                           <+> string "import"
                           <+> string (Model.ident p)

-- | Prettify a Language Import
prettyLangImport :: Patterns -> Doc
prettyLangImport ps = vsep $ map f ps
                      where
                        f p = string "import" <+> string (fromJust (Model.origin p))


-- ---------------------------------------------------------------- [ Patterns ]

-- | Prettify declared local patterns
prettyPatterns :: Patterns -> Doc
prettyPatterns [] = empty
prettyPatterns ps = vsep $ map prettyPattern (reverse ps)

-- | Prettify a Pattern
prettyPattern :: Pattern -> Doc
prettyPattern p = text (Model.ident p)
                  <+> string "<-"
                  <+> mod
                  <+> string "Pattern"
                  <> parens (dquotes (string (Model.name p)))
                  <> properties
                  where
                    mod = if isNothing (Model.modifier p)
                          then empty
                          else string (show (Model.modifier p))
                    properties = if hasProperties p
                                 then vcat [lbrace, prettyProperties p, rbrace]
                                 else empty


-- | Prettify a patterns properties
prettyProperties :: Pattern -> Doc
prettyProperties p = indent 4 (align (extends <$> implements))
                     where
                       extends = prettyIDList ":extends" (Model.extends p)
                       implements = prettyIDList ":implements" (Model.implements p)

-- --------------------------------------------------------------- [ Relations ]

-- | Prettify the relations of the Patterns
prettyRelations :: Patterns -> Doc
prettyRelations [] = empty
prettyRelations ps = vsep [requires, links]
                     where
                       requires = vsep $ map prettyRequires ps'
                       links = vsep $ map prettyLinks ps'
                       ps' = reverse ps

-- | Prettify a list of requires relations
prettyRequires :: Pattern -> Doc
prettyRequires p = prettyRelation (Model.ident p) "uses" (Model.requires p)

-- | Prettify a list of requires relations
prettyLinks :: Pattern -> Doc
prettyLinks p = prettyRelation (Model.ident p) "linkedTo" (Model.links p)



-- | Prettify a list of relations
prettyRelation :: ID -> String -> Maybe Relations -> Doc
prettyRelation _ _ Nothing = empty
prettyRelation from s (Just rs) = pretty121Relations from s r121
                                  <$> pretty12ManyRelations from s r12M
                                  where
                                    (r121, r12M) = break (isNothing . Model.desc) rs

-- | Prettify 1-2-Many Relations
pretty12ManyRelations :: String -> String -> Relations -> Doc
pretty12ManyRelations _ _ [] = empty
pretty12ManyRelations from s rs = string from <+> prettyIDList s (Just rs)


-- | Prettify 1-2-1 Relations
pretty121Relations :: String -> String -> Relations -> Doc
pretty121Relations from s [] = empty
pretty121Relations from s rs = vsep $ map (pretty121Relation from s) rs

-- | Prettify a 1-2-1 Relation
pretty121Relation :: String -> String -> Relation -> Doc
pretty121Relation from s r = string from
                             <+> string s
                             <+> string to
                             <+> desc
                             where
                               to = Model.to r
                               desc = if isNothing (Model.desc r)
                                      then empty
                                      else colon <+> string (fromJust (Model.desc r))

-- -------------------------------------------------------------------- [ Misc ]
-- | Prettify a list if IDs
prettyIDList :: String -> Maybe Relations -> Doc
prettyIDList _ Nothing = empty
prettyIDList s (Just rs) = string s
                           <+> list (prettyIDs ids)
                           where
                             ids = map Model.to rs
                             prettyIDs = map string

-- --------------------------------------------------------------------- [ EOF ]
