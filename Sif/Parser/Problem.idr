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

import Sif.Pattern

%default partial

-- --------------------------------------------------------------------- [ AST ]

data ProbASTty = ReqTy | ProbTy

data ProbAST : ProbASTty -> Type where
  MkReq : (ident : String)
       -> (ty    : RTy)
       -> (title : String)
       -> (desc  : Maybe String)
       -> ProbAST ReqTy
  MkProb : (ident : String)
        -> (title : String)
        -> (desc  : Maybe String)
        -> List (ProbAST ReqTy)
        -> ProbAST ProbTy

-- ----------------------------------------------------------------- [ Parsers ]


furpsTy : Parser RTy
furpsTy = (lexeme $ string "Functional"     *> return FUNC)
      <|> (lexeme $ string "Usability"      *> return USAB)
      <|> (lexeme $ string "Reliability"    *> return RELI)
      <|> (lexeme $ string "Performance"    *> return PERF)
      <|> (lexeme $ string "Supportability" *> return SUPP)
      <?> "Requirement Type"


requirement : Parser $ ProbAST ReqTy
requirement = do
    i <- ident
    token "<-"
    ty <- furpsTy
    t <- title
    d <- opt descString
    space
    pure $ MkReq i ty t d
  <?> "Requirement"


problem : Parser $ ProbAST ProbTy
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
    d <- opt desc
    rs <- many requirement
    space
    pure $ (MkProb i t d rs)
  <?> "Problem Specification"

-- --------------------------------------------------------------------- [ EOF ]
