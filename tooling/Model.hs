module Model where

-- -------------------------------------------------------- [ Pattern Language ]
data Plang = Plang {
      title    :: String,
      label    :: ID,
      imports  :: Maybe Imports,
      patterns :: Patterns
    } deriving (Show)

-- ----------------------------------------------------------------- [ Imports ]
data Import = Import {
      lang    :: ID,
      pattern :: Maybe Pattern
    } deriving (Show)

-- ----------------------------------------------------------------- [ Pattern ]
data Pattern = Pattern {
      name       :: String,
      ident      :: ID,
      modifier   :: Maybe Modifier,
      extends    :: Maybe Extends,
      implements :: Maybe Realises,
      requires   :: Maybe Requires,
      links      :: Maybe Links
    } deriving (Show, Eq)
    
-- --------------------------------------------------------------- [ Relations ]
data Relation = Relation {
      to   :: Pattern,
      desc :: Maybe String }
    deriving (Show, Eq)

data Modifier = Abstract | Integration
                deriving (Show, Eq, Read, Enum, Ord)

-- ------------------------------------------------------------ [ Type Aliases ]
-- PLang
type Imports  = [ Import ]
type Patterns = [ Pattern ]
-- Relations
type Extends  = [ Relation ]
type Realises = [ Relation ]
type Requires = [ Relation ]
type Links    = [ Relation ]
type Relations = [Relation]
-- Misc
type ID       = String
type IDs      = [ ID ]
-- --------------------------------------------------------------------- [ EOF ]
