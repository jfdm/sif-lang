-- | The parser.
module Parser (parseSif) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import Lexer
import Model.AST

-- ------------------------------------------- [ Pattern Language Model Parser ]

-- | Parses a Sif Spec file into the corresponding AST
parseSif :: String -> PlangExpr
parseSif fname =
    case runParser (runLex parsePlang) [] "" fname of
      Left err -> error (show err)
      Right ast -> ast

-- | Definition for a pattern language
-- parsePlang ::= parseMetadata parseImports parsePattern+ parseRelation*;
parsePlang :: Parser PlangExpr
parsePlang = do (title, label) <- parseMetadata
                optionMaybe parseImports
                reserved "patterns"
                many parsePattern
                reserved "relations"
                many parseRelation
                pState <- getState
                return (PlangExpr title label pState)
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
parseImports :: Parser ()
parseImports = do is <- liftM concat $ many1 parseImport
                  putState is
               <?> "Imports"

-- | Parse a single import
-- parseImport ::= parseImportM | parseImportLang
parseImport :: Parser PatternsExpr
parseImport = try parseImportM <|> parseImportLang <?> "Import"

-- | Import select patterns from a language
-- parseImportM ::= from <lang> import <idlist>;
parseImportM :: Parser PatternsExpr
parseImportM = do reserved "from"
                  lang <- identifier
                  reserved "import"
                  pIDs <- sepBy1 identifier comma
                  return $ map (`mkImportPattern` lang) pIDs
               <?> "Many Imports"

-- | Import a pattern language
-- parseImportLang ::= import <lang>;
parseImportLang :: Parser PatternsExpr
parseImportLang = do reserved "import"
                     lang <- identifier
                     return [mkImportPattern lang lang]
                  <?> "Language Import"

-- ----------------------------------------------- [ Pattern Parsing Functions ]

-- | Pattern Definition
-- parsePattern ::= parsePatternC | parsePatternS ;
parsePattern :: Parser ()
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"


-- | Complex patterns that extend or import other patterns
-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser ()
parsePatternC = do (id, typ, name) <- parsePatternHead
                   (extends, implements) <- braces parseProperties
                   let p = mkComplexPattern name id typ extends implements
                   modifyState (p :)
               <?> "Complex Pattern"

-- | Simple patterns.
-- parsePatternS ::= parsePatternHead ;
parsePatternS :: Parser ()
parsePatternS = do (id, typ, name) <- parsePatternHead
                   let p = mkSimplePattern name id typ
                   modifyState (p :)
               <?> "Simple Pattern"


-- | Common Head of a Pattern
-- parsePatternHead ::= <id> '<-' parseType? Pattern(<name>);
parsePatternHead :: Parser (ID, TyPattern, String)
parsePatternHead = do id <- identifier
                      reservedOp "<-"
                      typ <- optionMaybe parseType
                      let typ' = if isNothing typ
                                 then TyPattern
                                 else fromJust typ
                      reserved "Pattern"
                      name <- parens stringLiteral                    
                      return (id, typ', name)
                <?> "Pattern Head"

-- ---------------------------------------------- [ Pattern Modifier Functions ]

-- -- | Parse the modifiers
-- -- parseModifier ::= ("Integration" | "Abstract");
-- parseModifier :: Parser ModifierExpr
-- parseModifier = do try $ reserved "Abstract"
--                    return Abstract
--             <|> do reserved "Integration"
--                    return Integration
--             <?> "Modifier"

-- ---------------------------------------------------- [ Pattern Type Parsing ]
-- | Parse the type information
-- parseType ::= ("Component" | "System" | "Deployment"
--                 | "Admin" | "Implementation")
parseType :: Parser TyPattern
parseType = do try $ reserved "Component"
               return TyComponent
            <|> do reserved "System"
                   return TySystem
            <|> do reserved "Deployment"
                   return TyDeployment
            <|> do reserved "Admin"
                   return TyAdmin
            <|> do reserved "Implementation"
                   return TyImplementation
            <?> "Type"

-- ------------------------------------------------ [ Pattern Property Parsing ]

-- | Parse the properties
-- parseProperty ::= parseExtends parseImplements;
parseProperties :: Parser (Maybe RelationsExpr, Maybe RelationsExpr)
parseProperties = do extends <- optionMaybe parseExtends
                     implements <- optionMaybe parseImplements
                     return (extends, implements)
                  <?> "Properties"

-- parseImplements ::= ":implments" parseRelationIDs;
parseImplements :: Parser RelationsExpr
parseImplements = do reserved ":implements"
                     parseRelationIDs

-- parseExtends ::= ":extends" parseRelationIDss
parseExtends :: Parser RelationsExpr
parseExtends = do reserved ":extends"
                  parseRelationIDs

-- ---------------------------------------------- [ Relation Parsing Functions ]

-- @TODO Make better!

-- | Parse a relation
-- parseRelation ::= relationM | relation1 ;
parseRelation :: Parser ()
parseRelation = try parseRelationM <|> parseRelation1 <?> "Relations"

-- ----------------------------------- [ Functions for 1-Many Relation Parsing ]

-- relationM ::= <id> parseRelKWord <idlist>;
-- Nasty Code!
parseRelationM :: Parser ()
parseRelationM = do from <- identifier
                    kword <- parseRelKWord
                    tos <- parseIDList
                    ps <- getState
                    let p = getPattern from ps
                    if isNothing p
                    then fail $ "From Pattern in relation doesn't exist: " ++ from
                    else do let rs = map (\to -> tryMkRelation to ps Nothing) tos
                            if Nothing `elem` rs
                            then unexpected "Unknown Identity used in To relation"
                            else do let rs' = catMaybes rs
                                    let p' = if kword
                                             then addRequires rs' (fromJust p) 
                                             else addLinks rs' (fromJust p)
                                    modifyState (updatePatts p')
              <?> "1-2-Many Relation"

-- -------------------------------------- [ Functions for 1-1 Relation Parsing ]

-- parseRelation1 ::= <id> parseRelKWord <id> parseRelDesc?
-- Nasty Code!
parseRelation1 :: Parser ()            
parseRelation1 = do from <- identifier
                    kword <- parseRelKWord
                    to <- identifier
                    desc <- optionMaybe parseRelDesc
                    ps <- getState
                    let p = getPattern from ps
                    if isNothing p
                    then fail $ "From Pattern in relation doesn't exist: " ++ from
                    else do let r = tryMkRelation to ps desc
                            if isNothing r
                            then fail $ "To Pattern in relation doesn't exist: " ++ to
                            else do let p' = if kword
                                             then addRequire (fromJust r) (fromJust p)
                                             else addLink (fromJust r) (fromJust p)
                                    modifyState (updatePatts p')
                  <?> "1-2-1 Relation with Description"

-- -- -------------------------------------------------- [ Misc Parsing Functions ]

-- parseRelationIDs ::= <idlist>;
parseRelationIDs :: Parser RelationsExpr
parseRelationIDs = do ids <- parseIDs
                      ps <- getState
                      let exs = fmap (\id -> tryMkRelation id ps Nothing) ids
                      if Nothing `elem` exs
                      then unexpected "Unknown identity used"
                      else return $ catMaybes exs
               <?> "Relation ID List Parsing"

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

-- -- --------------------------------------------------------------------- [ EOF ]
