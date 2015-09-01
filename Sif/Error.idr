-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module Sif.Error

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

  CannotWriteFile : String -> SifError
  NoSuchBackend  : String -> SifError
  IDMissing      : String -> SifError
  ProblemMissing : String -> SifError
  DuplicateID    : String -> SifError
  FileMissing    : String -> SifError
  ParseError     : String -> String -> SifError

instance Show SifError where
  show IndexOutOfBounds = "Index Out of Bounds"
  show NoSuchPattern    = "No Such Pattern"
  show UnSuppFormat     = "Unsupported output format"
  show ResultInvalid    = "Invalid evaluation result"
  show NoSuchCommand    = "No such command"
  show ImportError      = "Import Error"
  show NoFileGiven      = "No File Given"
  show NoModeSpecified  = "Node Mode Specified"
  show NoFormatSpecified = "No output format specified"
  show FeatureNotImpl    = "Feature Not Implemented"
  show InternalErr       = "Internal Error"

  show (CannotWriteFile f) = "Cannot write file, " ++ show f
  show (IDMissing id)      = "Identifier Missing: " ++ show id
  show (ProblemMissing id) = "Problem Doesn't Exist: " ++ show id
  show (DuplicateID id)    = "Identifier already Exists: " ++ show id
  show (FileMissing fname) = "File Missing " ++ fname
  show (ParseError fn err) =
      unlines [ unwords ["Error Parsing file", show fn, "error was:"]
              , err]
  show (NoSuchBackend b)    = unwords ["No Such Backend", show b]
-- --------------------------------------------------------------------- [ EOF ]
