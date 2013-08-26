module Model where

-- -------------------------------------------------------- [ Pattern Language ]
data Plang = Plang {
      info     :: Metadata,
      imports  :: Maybe Imports,
      patterns :: Patterns
    } deriving (Show)

data Metadata = Metadata {
      title :: String,
      label :: ID
    } deriving (Show)

-- ----------------------------------------------------------------- [ Imports ]
data Import = Import {
      lang    :: ID,
      pattern :: Maybe Pattern
    } deriving (Show)

-- ----------------------------------------------------------------- [ Pattern ]
data Pattern = Pattern {
      name       :: String,
      identity   :: ID,
      modifier   :: Maybe Modifier,
      extends    :: Maybe Extends,
      implements :: Maybe Realises,
      requires   :: Maybe Requires,
      links      :: Maybe Links
    } deriving (Show)
    
-- --------------------------------------------------------------- [ Relations ]
data Relation = Relation {
      to   :: Pattern,
      desc :: Maybe String }
    deriving (Show)

-- ------------------------------------------------------------ [ Type Aliases ]
-- PLang
type Imports  = [ Import ]
type Patterns = [ Pattern ]
-- Relations
type Extends = [ Relation ]
type Realises = [ Relation ]
type Requires = [ Relation ]
type Links    = [ Relation ]
-- Misc
type Modifier = String
type ID       = String
type IDs      = [ ID ]
-- --------------------------------------------------------------------- [ EOF ]
