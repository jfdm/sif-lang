module Model where

type ID = String
type IDs = [ ID ]

type Patterns = [ Pattern ]
type Relations = [ Relation ]
type Imports = [ Import ]
type Modifier = String


data Plang = Plang {
      info :: Metadata,
      imports :: Maybe Imports,
      patterns :: Patterns,
      relations :: Relations
    } deriving (Show)

data Metadata = Metadata {
      title :: String,
      label :: ID
    } deriving (Show)

data Import = Import {
      lang :: ID,
      pattern :: Maybe ID
    } deriving (Show)

data Pattern = Pattern {
      name :: String,
      ident :: ID,
      extends :: Maybe IDs,
      implements :: Maybe IDs,
      modifier :: Maybe Modifier
    } deriving (Show)


data Relation =
    Requires { from :: ID,
               to :: IDs,
               desc :: Maybe String }
  | Links { from :: ID,
              to :: IDs,
              desc :: Maybe String }
    deriving (Show)


-- --------------------------------------------------------------------- [ EOF ]
