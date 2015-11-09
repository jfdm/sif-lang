-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Error

import Effects
import Effect.Exception

data SifError : Type where
  IndexOutOfBounds  : SifError
  NoSuchPattern     : SifError
  UnSuppFormat      : SifError
  ResultInvalid     : SifError
  NoSuchCommand     : SifError
  InternalErr       : SifError
  ImportError       : SifError
  NoFileGiven       : SifError
  NoModeSpecified   : SifError
  NoFormatSpecified : SifError
  FeatureNotImpl    : SifError
  ContextMismatch : SifError

  CannotWriteFile : String -> SifError
  NoSuchBackend   : String -> SifError
  IDMissing       : String -> SifError
  DuplicateID     : String -> SifError
  FileMissing     : String -> SifError
  ParseError      : String -> String -> SifError
  MismatchError   : String -> String -> SifError
  GeneralError    : String -> SifError

instance Show SifError where
  show IndexOutOfBounds  = "Index Out of Bounds"
  show NoSuchPattern     = "No Such Pattern"
  show UnSuppFormat      = "Unsupported output format"
  show ResultInvalid     = "Invalid evaluation result"
  show NoSuchCommand     = "No such command"
  show ImportError       = "Import Error"
  show NoFileGiven       = "No File Given"
  show NoModeSpecified   = "Node Mode Specified"
  show NoFormatSpecified = "No output format specified"
  show FeatureNotImpl    = "Feature Not Implemented"
  show InternalErr       = "Internal Error"
  show ContextMismatch   = "Solution Problem Context Mismatch"

  show (GeneralError m) = m

  show (CannotWriteFile f) = unwords ["Cannot write file:", show f]
  show (NoSuchBackend b)   = unwords ["No Such Backend:", show b]
  show (IDMissing id)      = unwords ["Identifier Missing:", show id]
  show (DuplicateID id)    = unwords ["Identifier already Exists:", show id]
  show (FileMissing fname) = unwords ["File Missing", show fname]

  show (ParseError fn err) =
      unlines [ unwords ["Error Parsing file", show fn, "error was:"]
              , err]

  show (MismatchError g e)  =
      unwords ["Given ID:", show g, "but got", show e, "instead."]


namespace Sif
  raise : SifError -> Eff b ['sif ::: EXCEPTION SifError]
  raise err = 'sif :- Exception.raise err

-- --------------------------------------------------------------------- [ EOF ]
