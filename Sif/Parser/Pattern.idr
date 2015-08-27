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
         -> (desc : Maybe String)
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
traitTy = (keyword "Advantage"    *> return ADV)
      <|> (keyword "Disadvantage" *> return DIS)
      <|> (keyword "Trait"        *> return DIS)
      <?> "Trait Type"

cValue : Parser CValue
cValue = (keyword "Makes"   *> return MAKES)
     <|> (keyword "Helps"   *> return HELPS)
     <|> (keyword "SomePos" *> return SOMEPOS)
     <|> (keyword "Unknown" *> return UNKNOWN)
     <|> (keyword "SomeNeg" *> return SOMENEG)
     <|> (keyword "Breaks"  *> return BREAK)
     <|> (keyword "Hurts"   *> return HURTS)
     <?> "CValue"

sValue : Parser SValue
sValue = (keyword "Denied"    *> return DENIED)
     <|> (keyword "WeakDen"   *> return WEAKDEN)
     <|> (keyword "WeakSatis" *> return WEAKSATIS)
     <|> (keyword "Satisfied" *> return SATISFIED)
     <|> (keyword "Undecided" *> return UNDECIDED)
     <|> (keyword "None"      *> return NONE)
     <?> "Trait Type"

affect : Parser $ SolAST AffectTy
affect = do
    c <- cValue
    space
    i <- ident
    space
    d <- opt $ (keyword "by" *> descString)
    pure $ Affects c i d
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

solution : Parser $ (Maybe String, SolAST SolTy)
solution = do
      keyword "sif"
      space
      string "solution"
      eol
      space
      pd <- opt desc
      keyword "Solution"
      t <- title
      keyword "solves"
      space
      pID <- ident
      (d,ps) <- braces $ body
      pure $ (pd, Solution t pID d ps)
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
