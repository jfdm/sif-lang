-- -------------------------------------------------------- [ Problem.idr<Sif> ]
-- Module    : Problem.idr<Sif>
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Parser problem specifications
module Sif.DSL.Parser.Problem

-- ----------------------------------------------------------------- [ Imports ]
import Lightyear
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


docString : Parser String
docString = do
    keyword "where"
    s <- descString
    pure s

variable : Parser a -> Parser $ Pair a VariableDecl
variable getTy = do
    i <- ident
    token "<-"
    ty <- getTy
    t <- title
    space
    d <- opt docString
    pure $ MkPair ty (MkVar i t d)
  <?> "Variable"

requirement : Parser $ SifAST tyREQ
requirement = do
    (ty, MkVar i t d) <- variable furpsTy
    space
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

public
problem : Parser $ SifAST tyPROBLEM
problem = do
    keyword "sif"
    string "problem"
    eol
    space
    (MkVar i t d) <- problemDef
    c <- opt context
    space
    rs <- many requirement
    space
    case c of
      Nothing => pure $ (AST.Problem i t d ("std", defaultDomain) rs)
      Just c' => pure $ (AST.Problem i t d c' rs)
  <?> "Problem Specification"

-- --------------------------------------------------------------------- [ EOF ]
