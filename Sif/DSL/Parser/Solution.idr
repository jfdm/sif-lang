-- ------------------------------------------------------------ [ Solution.idr ]
-- Module    : Solution.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Parser problem specifications
module Sif.DSL.Parser.Solution

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
import Lightyear.Strings

import GRL.Lang.GLang

import Sif.Types
import Sif.AbsSyntax

import Sif.DSL.Parser.Utils
import Sif.DSL.Parser.Common
import Sif.DSL.Parser.Problem

-- -------------------------------------------------------------- [ Directives ]
%default partial
%access private

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

affect : Parser $ SifAST tyAFFECTS
affect = do
    c <- cValue
    space
    i <- ident
    space
    d <- opt $ (keyword "by" *> descString)
    pure $ AST.Affect c i d
  <?> "Affects"

trait : Parser $ SifAST tyTRAIT
trait = do
      ty <- traitTy
      t <- title
      keyword "is"
      s <- sValue
      (d,as) <- braces body
      space
      pure $ AST.Trait ty t s d as
    <?> "Trait"
  where
    body : Parser (Maybe String, List $ SifAST tyAFFECTS)
    body = do
        d <- opt desc
        keyword "Affects"
        as <- braces $ commaSep1 affect
        space
        pure $ (d,as)
      <?> "Affects"


property : Parser $ SifAST tyPROPERTY
property = do
      keyword "Property"
      t <- title
      (d,ts) <- braces body
      space
      pure $ AST.Property t d ts
    <?> "Property"
  where
    body : Parser (Maybe String, List $ SifAST tyTRAIT)
    body = do
        d <- opt desc
        ts <- some trait
        space
        pure (d, ts)
      <?> "Property Body"

public
solution : Parser $ (SifAST tySOLUTION)
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
      keyword "in"
      cID <- ident
      (d,ps) <- braces $ body
      pure $ AST.Solution t (pID,pd) d cID ps
    <?> "Solution"
  where
    body : Parser (Maybe String, List $ SifAST tyPROPERTY)
    body = do
        d <- opt desc
        ps <- some property
        space
        pure (d, ps)
      <?> "Solution Body"



-- --------------------------------------------------------------------- [ EOF ]
