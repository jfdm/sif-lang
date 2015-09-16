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

furpsTy : Parser RTy
furpsTy = (keyword "Functional"     *> return FUNC)
      <|> (keyword "Usability"      *> return USAB)
      <|> (keyword "Reliability"    *> return RELI)
      <|> (keyword "Performance"    *> return PERF)
      <|> (keyword "Supportability" *> return SUPP)
      <?> "Requirement Type"

requirement : Parser $ SifAST tyREQ
requirement = do
    i <- ident
    token "<-"
    ty <- furpsTy
    t <- title
    d <- opt descString
    space
    pure $ AST.Req i ty t d
  <?> "Requirement"

context : String -> Parser $ Pair String SifDomain
context id = do
  keyword "where"
  keyword id
  keyword "<-"
  keyword "Context"
  t <- title
  desc <- opt descString
  pure $ MkPair id (MkDomain t desc)


public
problem : Parser $ SifAST tyPROBLEM
problem = do
    keyword "sif"
    string "problem"
    eol
    space
    keyword "Problem"
    t <- title
    keyword "as"
    i <- ident
    space
    keyword "in"
    cID <- ident
    space
    d <- opt desc
    rs <- many requirement
    space
    c <- context cID
    pure $ (AST.Problem i t d c rs)
  <?> "Problem Specification"

-- --------------------------------------------------------------------- [ EOF ]