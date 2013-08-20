module Model where

type ID = String
type IDs = [ ID ]

type Patterns = [ Pattern ]
type Relations = [ Relation ]
type Imports = [ Import ]
type Modifier = String


data PatternLang = PatternLang {
      info :: PLangLabel,
      imports :: Maybe Imports,
      patterns :: Patterns,
      relations :: Relations
    } deriving (Show)

data PLangLabel = PLangLabel {
      title :: String,
      label :: ID
    } deriving (Show)

data Import = Import {
      langID :: ID,
      patternID :: Maybe ID,
      alias :: Maybe ID
    } deriving (Show)

data Pattern = Pattern {
      name :: String,
      id :: ID,
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
