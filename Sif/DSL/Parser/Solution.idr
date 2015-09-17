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

affects : Parser $ List $ SifAST tyAFFECTS
affects = do
    keyword "Affects"
    as <- braces $ commaSep1 affect
    sifComment
    pure as

trait : Parser $ SifAST tyTRAIT
trait = do
      d <- opt sifDoc
      ty <- traitTy
      t <- title
      keyword "is"
      s <- sValue
      as <- braces affects
      sifComment
      pure $ AST.Trait ty t s d as
    <?> "Trait"

property : Parser $ SifAST tyPROPERTY
property = do
      d <- opt sifDoc
      keyword "Property"
      t <- title
      ts <- braces $ some trait
      sifComment
      pure $ AST.Property t d ts
    <?> "Property"

public
solution : Parser $ (SifAST tySOLUTION)
solution = do
      sifComment
      keyword "sif"
      space
      string "solution"
      eol
      sifComment
      pd <- opt desc
      d <- opt sifDoc
      keyword "Solution"
      t <- title
      keyword "solves"
      space
      pID <- ident
      keyword "in"
      cID <- ident
      ps <- braces $ some property
      pure $ AST.Solution t (pID,pd) d cID ps
    <?> "Solution"



-- --------------------------------------------------------------------- [ EOF ]
