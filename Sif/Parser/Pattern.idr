-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Parser problem specifications
module Sif.Parser.Problem

import Lightyear
import Lightyear.Strings

import Sif.Parser.Utils
import Sif.Parser.Common
import Sif.Parser.Problem

import Sif.Pattern

%default partial
-- --------------------------------------------------------------------- [ AST ]

data SolASTty = SolTy | PropTy | TraitTy | AffectTy

data SolAST : SolASTty -> Type where
  Affects : (value : CValue)
         -> (id : String)
         -> SolAST AffectTy
  Trait : TTy
       -> (title : String)
       -> (value : SValue)
       -> (desc  : Maybe String)
       -> (affects : List (SolAST AffectTy) )
       -> SolAST TraitTy
  Property : (title : String)
          -> (desc : Maybe String)
          -> (traits : List (SolAST TraitTy))
          -> SolAST PropTy
  Solution : (title : String)
          -> (probID : String)
          -> (desc : Maybe String)
          -> (properties : List (SolAST PropTy))
          -> SolAST SolTy

-- ----------------------------------------------------------------- [ Parsers ]
traitTy : Parser TTy
traitTy = (lexeme $ string "Advantage"    *> return ADV)
      <|> (lexeme $ string "Disadvantage" *> return DIS)
      <?> "Trait Type"

cValue : Parser CValue
cValue = (lexeme $ string "Makes"   *> return MAKES)
     <|> (lexeme $ string "Helps"   *> return HELPS)
     <|> (lexeme $ string "SomePos" *> return SOMEPOS)
     <|> (lexeme $ string "Unknown" *> return UNKNOWN)
     <|> (lexeme $ string "SomeNeg" *> return SOMENEG)
     <|> (lexeme $ string "Breaks"  *> return BREAK)
     <|> (lexeme $ string "Hurts"   *> return HURTS)
     <?> "CValue"

sValue : Parser SValue
sValue = (lexeme $ string "Denied"    *> return DENIED)
     <|> (lexeme $ string "WeakDen"   *> return WEAKDEN)
     <|> (lexeme $ string "WeakSatis" *> return WEAKSATIS)
     <|> (lexeme $ string "Satisfied" *> return SATISFIED)
     <|> (lexeme $ string "Undecided" *> return UNDECIDED)
     <|> (lexeme $ string "None"      *> return NONE)
     <?> "Trait Type"

affect : Parser $ SolAST AffectTy
affect = do
    c <- cValue
    space
    i <- ident
    pure $ Affects c i
  <?> "Affects"

trait : Parser $ SolAST TraitTy
trait = do
      ty <- traitTy
      t <- title
      keyword "is"
      s <- sValue
      (d,as) <- braces body
      space
      pure $ Trait ty t s d as
    <?> "Trait"
  where
    body : Parser (Maybe String, List $ SolAST AffectTy)
    body = do
        d <- opt desc
        keyword "Affects"
        as <- braces $ commaSep1 affect
        space
        pure $ (d,as)
      <?> "Affects"


property : Parser $ SolAST PropTy
property = do
      keyword "Property"
      t <- title
      (d,ts) <- braces body
      space
      pure $ Property t d ts
    <?> "Property"
  where
    body : Parser (Maybe String, List $ SolAST TraitTy)
    body = do
        d <- opt desc
        ts <- some trait
        space
        pure (d, ts)
      <?> "Property Body"

solution : Parser $ ((String, Maybe String), SolAST SolTy)
solution = do
      keyword "sif"
      space
      keyword "solution"
      space
      keyword "solves"
      pID <- ident
      keyword "makes"
      space
      keyword "pattern"
      pt <- title
      space
      pd <- opt desc
      keyword "Solution"
      t <- title
      (d,ps) <- braces $ body
      pure $ ((pt,pd), Solution t pID d ps)
    <?> "Solution"
  where
    body : Parser (Maybe String, List $ SolAST PropTy)
    body = do
        d <- opt desc
        ps <- some property
        space
        pure (d, ps)
      <?> "Solutino Body"



-- --------------------------------------------------------------------- [ EOF ]
