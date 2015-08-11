-- ------------------------------------------------------------ [ Commands.idr ]
-- Module    : Commands.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Commands

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
  Help        : SifCMD

showHelp : String
showHelp = """
Command                 | Description
------------------------|-------------------------------------------------------
:list                   | List library contents
:show n                 | Show pattern identified by index.
:eval n                 | Evaluate pattern identified by index.
:showAs n fmt           | Show pattern using given format.
:saveAs n fmt fname     | Save pattern in given format to file specified.
:chkExtPattern probFile | Check externally defined pattern.
               solFile  |
:quit :q :exit          | Quit the repl
:? :help                | Show this help
"""

covering
getCmdIndex : SifCMD -> Maybe Nat
getCmdIndex Help                  = Nothing
getCmdIndex Quit                  = Nothing
getCmdIndex ListLib               = Nothing
getCmdIndex (ShowPattern n _ _)   = Just n
getCmdIndex (EvalPattern n)       = Just n
getCmdIndex (CheckExtPattern _ _) = Nothing

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
    string ":saveAs"
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
help : Parser SifCMD
help = (string ":?"    *> return Help)
   <|> (string ":help" *> return Help)

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
  <|> help
  <?> "Command"

public
parseCMD : String -> Either String SifCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
