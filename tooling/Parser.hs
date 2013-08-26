module Parser (parseSif) where

import Control.Monad.State
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Prim (parseTest)
import Lexer
import Model
import Utils

-- ------------------------------------------- [ Pattern Language Model Parser ]

-- | Parses a Sif Spec file into the corresponding AST
parseSif :: String -> Plang
parseSif fname =
    case parse (runLex parsePlang) "" fname of
      Left err -> error (show err)
      Right ast -> ast

-- | Definition for a pattern language
-- parsePlang ::= parseMetadata parseImports parsePattern+ parseRelation*;
parsePlang :: Parser Plang
parsePlang = do mdata <- parseMetadata
                imports <- optionMaybe parseImports
                reserved "patterns"
                patterns <- manyTill parsePattern (reserved "relations")
                relations <- many1 parseRelation
                return (Plang mdata imports patterns)
             <?> "Language Instance"

-- ---------------------------------------------------- [ Language Declaration ]

-- | Definition for a pattern language metadata.
-- parseLangDecl ::= language <title> as <id>
parseMetadata :: Parser Metadata
parseMetadata = do reserved "language"
                   title <- stringLiteral
                   reserved "as"
                   id <- identifier
                   return (Metadata title id)
                <?> "Language Declaration"

-- ------------------------------------------------- [ Import Parsing Function ]

-- | Definition of language imports
-- parseImports ::= parseImport*;
parseImports :: Parser Imports
parseImports = do is <- many1 parseImport
                  map (\x -> modify ((Model.pattern x) :)) is
                  return $ concat is
               <?> "Imports"

-- | Parse a single import
-- parseImport ::= parseImportM | parseImportLang
parseImport :: Parser Imports
parseImport = try parseImportM <|> parseImportLang <?> "Import"

-- | Import select patterns from a language
-- parseImportM ::= from <lang> import <idlist>;
parseImportM :: Parser Imports
parseImportM = do reserved "from"
                  lang <- identifier
                  reserved "import"
                  ps <- sepBy1 identifier comma
                  return $ map (\x -> Import lang (Pattern x x Nothing Nothing Nothing Nothing Nothing)) ps
               <?> "Many Imports"

-- | Import a pattern language
-- parseImportLang ::= import <lang>;
parseImportLang :: Parser Imports
parseImportLang = do reserved "import"
                     lang <- identifier
                     return (Import lang Nothing)
                  <?> "Language Import"

-- ----------------------------------------------- [ Pattern Parsing Functions ]

-- | Pattern Definition
-- parsePattern := parsePatternSimple | parsePatternC
parsePattern :: Parser Pattern
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"


-- | Complex patterns that extend or import other patterns
-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser Pattern
parsePatternC = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens stringLiteral
                   (extends, implements) <- braces parseProperties
                   p <- (Pattern name id modifier extends implements Nothing Nothing)
                   modify (p :)
                   return p
               <?> "Complex Pattern"

-- | Simple patterns.
-- parsePatternS ::= <id> <- <modifier> Pattern(<name>) ;
parsePatternS :: Parser Pattern
parsePatternS = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens stringLiteral
                   p <- (Pattern name id modifier Nothing Nothing Nothing)
                   modify (p :)
                   return p
               <?> "Simple Pattern"

-- ---------------------------------------------- [ Pattern Modifier Functions ]

-- | Parse the modifiers
-- parseModifier ::= parseModifierA | parseModifierI;
parseModifier :: Parser Modifier
parseModifier = try parseModifierA <|> parseModifierI <?> "Modifier"

parseModifierA :: Parser Modifier
parseModifierA = do reserved "Abstract"
                    return "Abstract"
                 <?> "Abstract"

parseModifierI :: Parser Modifier
parseModifierI = do reserved "Integration"
                    return "Integration"
                 <?> "Integration"

-- ------------------------------------------------ [ Pattern Property Parsing ]

-- | Parse the properties
-- parseProperty ::= parseImplements parseExtends;
parseProperties :: Parser (Maybe Extends, Maybe Realises)
parseProperties = do extends <- optionMaybe parseExtends
                     implements <- optionMaybe parseImplements
                     return (extends, implements)
                 <?> "Properties"

-- parseExtends ::= :extends <idlist>;
parseExtends :: Parser Extends
parseExtends = do reserved ":extends"
                  ids <- parseIDs
                  st <- get
                  map (\id -> Relation (getPattern id st) Nothing) ids
               <?> "Specialisation"

-- parseImplements ::= :implements <idlist>;
parseImplements :: Parser Realises
parseImplements = do reserved ":implements"
                     parseIDs
                     ids <- parseIDs
                     st <- get
                     map (\id -> Relation (getPattern id st) Nothing) ids
                  <?> "Realisation"

-- ---------------------------------------------- [ Relation Parsing Functions ]

-- @TODO Make better!

-- | Parse a relation
-- parseRelation ::= relationM | relation1 ;
parseRelation :: Parser Relation
parseRelation = try parseRelation1 <|> parseRelationM <?> "Relations"

-- ----------------------------------- [ Functions for 1-Many Relation Parsing ]

-- TODO Write code to modify state table updating the `to' patterns with the relations.

-- relationM ::= relationMu | relationMl
parseRelationM :: Parser Relation
parseRelationM =  try parseRelationMu <|> parseRelationMl
                  <?> "1-2-Many Relation"

-- relationMu ::= <id> "uses" <idlist>;
parseRelationMu :: Parser Relation
parseRelationMu = do from <- identifier
                     reserved "uses"
                     to <- parseIDs
                     return (Requires from to (Just "uses")) -- Nasty Hack

-- relationMl ::= <id> "linkedTo" <idlist>;
parseRelationMl :: Parser Relation
parseRelationMl = do from <- identifier
                     reserved "linkedTo"
                     to <- parseIDs
                     return (Links from to (Just "linkedTo")) -- Nasty Hack

-- -------------------------------------- [ Functions for 1-1 Relation Parsing ]

-- parseRelation1 ::= relation1u | relation1l ;
parseRelation1 :: Parser Relation            
parseRelation1 = try parseRelation1u <|> parseRelation1l <?> "1-2-1 Relation with Description"
 
-- relation1u ::= <id> "uses" <id> ":" <desc>;                    
parseRelation1u :: Parser Relation
parseRelation1u = do from <- identifier
                     reserved "uses"
                     to <- parseID1
                     reservedOp ":"
                     desc <- optionMaybe stringLiteral
                     st <- get
                     res <- Relation (getPattern to st) desc
                     ust <- addLink from res st
                     put ust
                     return res

-- relation1l ::= <id> "linkedTo" <id> ":" <desc>;
parseRelation1l :: Parser Relation
parseRelation1l = do from <- identifier
                     reserved "linkedTo"
                     to <- parseID1
                     reservedOp ":"
                     desc <- optionMaybe stringLiteral
                     st <- get
                     return Relation (getPattern to st) desc

-- -------------------------------------------------- [ Misc Parsing Functions ]

-- parseIDs ::= <id> | <idlist>;
parseIDs :: Parser IDs
parseIDs = try parseIDList <|> parseID1 <?> "ID Lists"

-- parseID1 ::= <id>;
parseID1 :: Parser IDs
parseID1 = do id <- identifier
              return (id : [])

-- parseIDList ::= [ <id> (<id> ',')* ];
parseIDList :: Parser IDs
parseIDList = brackets $ sepBy1 identifier comma

-- ------------------------------------------------- [ Helper Testing Function ]

-- Test some parseExpression p with a given string s
testParseStr :: Parser -> String -> IO ()
testParseStr p s = parseTest p s

-- Test some parseExpression p with a given file f
testParseFile :: Show a => Parser a -> String -> IO ()
testParseFile p f = do { res <- parseFromFile (runLex p) f
                       ; case res of
                           Left err -> error (show err)
                           Right x -> print x
                       }


-- --------------------------------------------------------------------- [ EOF ]
