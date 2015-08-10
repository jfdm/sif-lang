-- ------------------------------------------------------------ [ Commands.idr ]
-- Module    : Commands.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Viewer.Commands

import Lightyear
import Lightyear.Strings

import Sif.Parser.Common
import Sif.Parser.Utils

import Sif.Pattern

%access public
-- ---------------------------------------------------------------- [ Commands ]

public
data SifCMD : Type where
  ShowPattern : Nat -> Maybe SifOutFormat -> Maybe String -> SifCMD
  ListLib     : SifCMD
  EvalPattern : Nat -> SifCMD
  CheckExtPattern : String -> String -> SifCMD
  Quit        : SifCMD

covering
getCmdIndex : SifCMD -> Maybe Nat
getCmdIndex (CheckExtPattern _ _) = Nothing
getCmdIndex Quit = Nothing
getCmdIndex ListLib = Nothing
getCmdIndex (ShowPattern n _ _) = Just n
getCmdIndex (EvalPattern n)     = Just n

private
cmdShowPattern : Parser SifCMD
cmdShowPattern = do
    string ":show"
    space
    i <- integer
    pure $ ShowPattern (cast $ abs i) (Just ORG) Nothing
  <?> "Show Pattern"

private
cmdShowAs : Parser SifCMD
cmdShowAs = do
    string ":showAs"
    space
    i <- integer
    space
    fmt <- ident
    pure $ ShowPattern (cast $ abs i) (readOutFMT fmt) Nothing
  <?> "Save Pattern"


private
cmdListLib : Parser SifCMD
cmdListLib = do
    string ":list"
    pure ListLib
  <?> "List libs"

private
cmdSavePattern : Parser SifCMD
cmdSavePattern = do
    string ":save"
    space
    i <- integer
    space
    fmt <- ident
    space
    fname <- ident
    pure $ ShowPattern (cast $ abs i) (readOutFMT fmt) (Just fname)
  <?> "Save Pattern"

private
cmdEvalPattern : Parser SifCMD
cmdEvalPattern = do
    string ":eval"
    space
    i <- integer
    pure $ EvalPattern (cast $ abs i)
  <?> "Eval Command"

private
cmdQuit : Parser SifCMD
cmdQuit = (string ":q"    *> return Quit)
      <|> (string ":quit" *> return Quit)
      <|> (string ":exit" *> return Quit)

private
chkExtPattern : Parser SifCMD
chkExtPattern = do
    keyword ":chkPattern"
    p <- literallyBetween '"'
    space
    s <- literallyBetween '"'
    pure $ CheckExtPattern p s

private
cmd : Parser SifCMD
cmd = cmdShowPattern
  <|> cmdListLib
  <|> cmdShowAs
  <|> cmdSavePattern
  <|> cmdEvalPattern
  <|> cmdQuit
  <|> chkExtPattern
  <?> "Command"

public
parseCMD : String -> Either String SifCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
