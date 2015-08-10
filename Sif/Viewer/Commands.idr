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
  ShowPattern : Nat -> SifCMD
  ListLib     : SifCMD
  SavePattern : Nat -> String -> String -> SifCMD
  EvalPattern : Nat -> SifCMD
  Quit        : SifCMD

private
cmdShowPattern : Parser SifCMD
cmdShowPattern = do
    string ":show"
    space
    i <- integer
    pure $ ShowPattern (cast $ abs i)
  <?> "Show Pattern"

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
    pure $ SavePattern (cast $ abs i) fmt fname
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
cmd : Parser SifCMD
cmd = cmdShowPattern
  <|> cmdListLib
  <|> cmdSavePattern
  <|> cmdEvalPattern
  <|> cmdQuit
  <?> "Command"

public
parseCMD : String -> Either String SifCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
