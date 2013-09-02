module Model.AST where

-- AST for the Pattern Language

-- -------------------------------------------------------- [ Pattern Language ]
data Plang = Plang {
      title    :: String,
      label    :: ID,
      patterns :: Patterns
    } deriving (Show)

-- ----------------------------------------------------------------- [ Pattern ]
data Pattern = Pattern {
      name       :: String,
      ident      :: ID,
      origin     :: Maybe String,
      modifier   :: Maybe Modifier,
      extends    :: Maybe Extends,
      implements :: Maybe Realises,
      requires   :: Maybe Requires,
      links      :: Maybe Links
    } deriving (Show, Eq)
    
-- --------------------------------------------------------------- [ Relations ]
data Relation = Relation {
      to   :: ID,
      desc :: Maybe String }
    deriving (Show, Eq)

data Modifier = Abstract | Integration
                deriving (Show, Eq, Read, Enum, Ord)

-- ------------------------------------------------------------ [ Type Aliases ]
-- PLang
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
