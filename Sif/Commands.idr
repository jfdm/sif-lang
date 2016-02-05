-- ------------------------------------------------------------ [ Commands.idr ]
-- Module    : Commands.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Commands

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Sif.Types
import Sif.DSL.Parser.Common
import Sif.DSL.Parser.Utils

-- -------------------------------------------------------------- [ DIrectives ]

%default partial
%access private

-- ---------------------------------------------------------------- [ Commands ]
public export
data SifCMD : Type where
  ShowPattern : Nat -> Maybe SifOutFormat -> Maybe String -> SifCMD
  ListLib     : SifCMD
  EvalPattern : Nat -> SifCMD
  CheckExtPattern : String -> String -> SifCMD
  PreludeLoad : Maybe String -> SifCMD
  Quit        : SifCMD
  Help        : SifCMD


export
showHelp : String
showHelp = """
Command                 | Description
------------------------|-------------------------------------------------------
:list                   | List library contents
:display n              | Show pattern identified by index.
:eval n                 | Evaluate pattern identified by index.
:convert n fmt          | Show pattern using given format.
:save n fmt "fname"     | Save pattern in given format to file specified.
:chkPattern "probFile"  | Check externally defined pattern.
            "solFile"   |
:load "dirname"         | Replace prelude.
:reload                 | Reload prelude.
:quit :q :exit          | Quit the repl
:? :help                | Show this help
"""

covering
export
getCmdIndex : SifCMD -> Maybe Nat
getCmdIndex Help                  = Nothing
getCmdIndex Quit                  = Nothing
getCmdIndex ListLib               = Nothing
getCmdIndex (ShowPattern n _ _)   = Just n
getCmdIndex (EvalPattern n)       = Just n
getCmdIndex (CheckExtPattern _ _) = Nothing
getCmdIndex (PreludeLoad _)       = Nothing

display : Parser SifCMD
display = do
    string ":display"
    space
    i <- integer
    pure $ ShowPattern (cast $ abs i) (Just COMPACT) Nothing
  <?> "Show Pattern"

convert : Parser SifCMD
convert = do
    string ":convert"
    space
    i <- integer
    space
    fmt <- ident
    pure $ ShowPattern (cast $ abs i) (readOutFMT fmt) Nothing
  <?> "Save Pattern"


list : Parser SifCMD
list = do
    string ":list"
    pure ListLib
  <?> "List libs"

save : Parser SifCMD
save = do
    string ":save"
    space
    i <- integer
    space
    fmt <- ident
    space
    fname <- ident
    pure $ ShowPattern (cast $ abs i) (readOutFMT fmt) (Just fname)
  <?> "Save Pattern"

eval : Parser SifCMD
eval = do
    string ":eval"
    space
    i <- integer
    pure $ EvalPattern (cast $ abs i)
  <?> "Eval Command"

quit : Parser SifCMD
quit = (string ":q"    *> return Quit)
      <|> (string ":quit" *> return Quit)
      <|> (string ":exit" *> return Quit)

help : Parser SifCMD
help = (string ":?"    *> return Help)
   <|> (string ":help" *> return Help)

check : Parser SifCMD
check = do
    keyword ":check"
    p <- quoted '"'
    space
    s <- quoted '"'
    pure $ CheckExtPattern p s

load : Parser SifCMD
load = (do keyword ":load"; dir <- quoted '"'; return (PreludeLoad (Just dir)) )
    <|> (keyword ":r" *> return (PreludeLoad Nothing))
    <|> (keyword ":reload" *> return (PreludeLoad Nothing))
    <?> "Loading"

cmd : Parser SifCMD
cmd = display
  <|> list
  <|> convert
  <|> save
  <|> eval
  <|> quit
  <|> load
  <|> check
  <|> help
  <?> "Command"

export
parseCMD : String -> Either String SifCMD
parseCMD s = parse cmd s

-- --------------------------------------------------------------------- [ EOF ]
