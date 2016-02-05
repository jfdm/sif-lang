-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Parser problem specifications
module Sif.DSL.Parser.Problem

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Sif.Types
import Sif.AbsSyntax
import Sif.Pattern.Model

import Sif.DSL.Parser.Utils
import Sif.DSL.Parser.Common

-- -------------------------------------------------------------- [ Directives ]

%default partial
%access private

-- ----------------------------------------------------------------- [ Parsers ]


data VariableDecl : Type where
  MkVar : String -> String -> (Maybe String) -> VariableDecl

furpsTy : Parser RTy
furpsTy = (keyword "Functional"     *> return FUNC)
      <|> (keyword "Usability"      *> return USAB)
      <|> (keyword "Reliability"    *> return RELI)
      <|> (keyword "Performance"    *> return PERF)
      <|> (keyword "Supportability" *> return SUPP)
      <?> "Requirement Type"


variable : Parser a -> Parser $ Pair a VariableDecl
variable getTy = do
    d <- opt sifDoc
    i <- ident
    token "<-"
    ty <- getTy
    t <- title
    sifComment
    pure $ MkPair ty (MkVar i t d)
  <?> "Variable"

requirement : Parser $ SifAST TyREQ
requirement = do
    (ty, MkVar i t d) <- variable furpsTy
    spaces
    pure $ AST.Req i ty t d
  <?> "Requirement"


context : Parser $ Pair String SifDomain
context = do
    (_, MkVar i t d) <- variable (keyword "Context")
    pure $ MkPair i $ MkDomain t d
  <?> "Context"

problemDef : Parser $ VariableDecl
problemDef = do
    (_, var) <- variable (keyword "Problem")
    pure var
  <?> "Problem"

export
problem : Parser $ SifAST TyPROBLEM
problem = do
    sifComment
    keyword "sif"
    string "problem"
    endOfLine
    sifComment
    (MkVar i t d) <- problemDef
    c <- opt $ some context
    sifComment
    rs <- many requirement
    sifComment
    case c of
      Nothing  => pure $ (AST.Problem i t d [("std", defaultDomain)] rs)
      Just Nil => pure $ (AST.Problem i t d [("std", defaultDomain)] rs)
      Just cs  => pure $ (AST.Problem i t d cs rs)
  <?> "Problem Specification"

-- --------------------------------------------------------------------- [ EOF ]
