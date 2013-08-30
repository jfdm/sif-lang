module Parser (parseSif) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import Lexer
import Model
import Utils

-- ------------------------------------------- [ Pattern Language Model Parser ]

-- | Parses a Sif Spec file into the corresponding AST
parseSif :: String -> Plang
parseSif fname =
    case runParser (runLex parsePlang) [] "" fname of
      Left err -> error (show err)
      Right ast -> ast

-- | Definition for a pattern language
-- parsePlang ::= parseMetadata parseImports parsePattern+ parseRelation*;
parsePlang :: Parser Plang
parsePlang = do (title, label) <- parseMetadata
                imports <- optionMaybe parseImports
                reserved "patterns"
                many parsePattern
                reserved "relations"
                many parseRelation
                pState <- getState
                let ps = filter (\p -> Model.ident p /= Model.name p) pState
                return (Plang title label imports ps)
             <?> "Language Instance"

-- ---------------------------------------------------- [ Language Declaration ]

-- | Definition for a pattern language metadata.
-- parseLangDecl ::= language <title> as <id>
parseMetadata :: Parser (String, ID)
parseMetadata = do reserved "language"
                   title <- stringLiteral
                   reserved "as"
                   label <- identifier
                   return (title, label)
                <?> "Language Declaration"

-- ------------------------------------------------- [ Import Parsing Function ]

-- | Definition of language imports
-- parseImports ::= parseImport*;
parseImports :: Parser Imports
parseImports = do is <- liftM concat $ many1 parseImport
                  putState $ mapMaybe Model.pattern is
                  return is
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
                  pIDs <- sepBy1 identifier comma
                  return $ map (\i -> Import lang (Just $ Pattern i i Nothing Nothing Nothing Nothing Nothing)) pIDs
               <?> "Many Imports"

-- | Import a pattern language
-- parseImportLang ::= import <lang>;
parseImportLang :: Parser Imports
parseImportLang = do reserved "import"
                     lang <- identifier
                     return [Import lang Nothing]
                  <?> "Language Import"

-- ----------------------------------------------- [ Pattern Parsing Functions ]

-- | Pattern Definition
-- parsePattern ::= parsePatternC | parsePatternS ;
parsePattern :: Parser ()
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"


-- | Complex patterns that extend or import other patterns
-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser ()
parsePatternC = do (id, modifier, name) <- parsePatternHead
                   (extends, implements) <- braces parseProperties
                   let p = Pattern name id modifier extends implements Nothing Nothing
                   modifyState (p :)
               <?> "Complex Pattern"

-- | Simple patterns.
-- parsePatternS ::= parsePatternHead ;
parsePatternS :: Parser ()
parsePatternS = do (id, modifier, name) <- parsePatternHead
                   let p = Pattern name id modifier Nothing Nothing Nothing Nothing
                   modifyState (p :)
               <?> "Simple Pattern"


-- | Common Head of a Pattern
-- parsePatternHead ::= <id> '<-' parseModifier? Pattern(<name>);
parsePatternHead :: Parser (ID, Maybe Modifier, String)
parsePatternHead = do id <- identifier
                      reservedOp "<-"
                      modifier <- optionMaybe parseModifier
                      reserved "Pattern"
                      name <- parens stringLiteral                    
                      return (id, modifier, name)
                <?> "Pattern Head"

-- ---------------------------------------------- [ Pattern Modifier Functions ]

-- | Parse the modifiers
-- parseModifier ::= ("Integration" | "Abstract");
parseModifier :: Parser Modifier
parseModifier = do try $ reserved "Abstract"
                   return Abstract
            <|> do reserved "Integration"
                   return Integration
            <?> "Modifier"

-- ------------------------------------------------ [ Pattern Property Parsing ]

-- | Parse the properties
-- parseProperty ::= parseImplements parseExtends;
parseProperties :: Parser (Maybe Extends, Maybe Realises)
parseProperties = do extends <- optionMaybe parseProperty
                     implements <- optionMaybe parseProperty
                     return (extends, implements)
                 <?> "Properties"

-- parseProperty ::= parsePropKWord <idlist>;
parseProperty :: Parser Relations
parseProperty = do parsePropKWord
                   ids <- parseIDs
                   ps <- getState
                   let exs = fmap (\id -> tryMkRelation id ps Nothing) ids
                   if Nothing `elem` exs
                   then unexpected "Unknown identity used"
                   else return $ catMaybes exs
               <?> "Properties Parsing"

-- ---------------------------------------------- [ Relation Parsing Functions ]

-- @TODO Make better!

-- | Parse a relation
-- parseRelation ::= relationM | relation1 ;
parseRelation :: Parser ()
parseRelation = try parseRelation1 <|> parseRelationM <?> "Relations"

-- ----------------------------------- [ Functions for 1-Many Relation Parsing ]

-- relationM ::= <id> parseRelKWord <idlist>;
parseRelationM :: Parser ()
parseRelationM = do from <- identifier
                    kword <- parseRelKWord
                    tos <- parseIDList
                    ps <- getState
                    if isNothing $ getPattern from ps
                    then fail $ "From Pattern in relation doesn't exist: " ++ from
                    else do let rels = map (\to -> tryMkRelation to ps Nothing) tos
                            if Nothing `elem` rels
                            then unexpected "Unknown Identity used in To relation"
                            else putState (last 
                                           (if kword -- below is a nasty hack need to sort
                                            then map (\r -> addRequire from r ps) (catMaybes rels)
                                            else map (\r -> addLink from r ps) (catMaybes rels)))
                  <?> "1-2-Many Relation"

-- -------------------------------------- [ Functions for 1-1 Relation Parsing ]

-- parseRelation1 ::= <id> parseRelKWord <id> parseRelDesc?                    
parseRelation1 :: Parser ()            
parseRelation1 = do from <- identifier
                    kword <- parseRelKWord
                    to <- identifier
                    desc <- optionMaybe parseRelDesc
                    ps <- getState
                    if isNothing $ getPattern from ps
                    then fail $ "From Pattern in relation doesn't exist: " ++ from
                    else do let res = tryMkRelation to ps desc
                            if isNothing res
                            then fail $ "To Pattern in relation doesn't exist: " ++ to
                            else putState
                                     (if kword
                                      then addRequire from (fromJust res) ps
                                      else addLink from (fromJust res) ps)
                  <?> "1-2-1 Relation with Description"

-- -- -------------------------------------------------- [ Misc Parsing Functions ]

-- parsePropKWord :: = (":extends" | ":implements");
parsePropKWord :: Parser Bool
parsePropKWord = do try $ reserved ":extends"
                    return True
             <|> do reserved ":implements"
                    return False
             <?> "Property Keyword"

-- parseRelKWord ::= ("uses" | "linkedTo" );
parseRelKWord :: Parser Bool
parseRelKWord = do try $ reserved "uses"
                   return True
            <|> do reserved "linkedTo"
                   return False
            <?> "Relation Keyword"

-- parseRelDesc ::= ":" <desc>
parseRelDesc :: Parser String
parseRelDesc = do reservedOp ":"
                  stringLiteral
               <?> "Relation Description"

-- parseIDs ::= <id> | <idlist>;
parseIDs :: Parser IDs
parseIDs = try parseIDList <|> parseID1 <?> "ID Lists"

-- parseID1 ::= <id>;
parseID1 :: Parser IDs
parseID1 = do id <- identifier
              return [id]

-- parseIDList ::= [ <id> (<id> ',')* ];
parseIDList :: Parser IDs
parseIDList = brackets $ sepBy1 identifier comma

-- -- ------------------------------------------------- [ Helper Testing Function ]

-- -- Test some parseExpression p with a given file f
-- testParseFile :: Show a => Parser a -> String -> IO ()
-- testParseFile p f = do { res <- parseFromFile (runLex p) f
--                        ; case res of
--                            Left err -> error (show err)
--                            Right x -> print x
--                        }


-- -- --------------------------------------------------------------------- [ EOF ]
