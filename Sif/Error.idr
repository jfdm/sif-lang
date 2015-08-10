-- --------------------------------------------------------------- [ Error.idr ]
-- Module    : Error.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]

module Sif.Error

data SifError : Type where
  IndexOutOfBounds : SifError
  NoSuchPattern : SifError
  UnSuppFormat  : SifError
  ResultInvalid : SifError
  NoSuchCommand : SifError
  InternalErr   : SifError
  ImportError   : SifError
  IDMissing      : String -> SifError
  ProblemMissing : String -> SifError
  DuplicateID   : String -> SifError
  FileMissing   : String -> SifError
  ParseError    : String -> SifError

instance Show SifError where
  show IndexOutOfBounds = "Index Out of Bounds"
  show NoSuchPattern    = "No Such Pattern"
  show UnSuppFormat     = "Unsupported output format"
  show ResultInvalid    = "Invalid evaluation result"
  show NoSuchCommand    = "No such command"
  show ImportError      = "Import Error"
  show (IDMissing id)      = "Identifier Missing: " ++ show id
  show (ProblemMissing id) = "Problem Doesn't Exist: " ++ show id
  show (DuplicateID id)    = "Identifier already Exists: " ++ show id
  show (FileMissing fname) = "File Missing " ++ fname
  show (ParseError  err)   = "ParseError:\n" ++ err


-- --------------------------------------------------------------------- [ EOF ]
