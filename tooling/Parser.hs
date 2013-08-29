module Parser (parseSif) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import Lexer
import Model
import Utils

--  Need to added proper checks when making the relations!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  It is the whole goddam point of a stateful parser!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  Need to added proper checks when making the relations!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  It is the whole goddam point of a stateful parser!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--  mkRelation (from Utils) is a nasty hack. Try and improve using parser errors, otherwise what is the point of using state.

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
                many1 parsePattern
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
-- parsePattern := parsePatternSimple | parsePatternC
parsePattern :: Parser ()
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"


-- | Complex patterns that extend or import other patterns
-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser ()
parsePatternC = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens stringLiteral
                   (extends, implements) <- braces parseProperties
                   let p = Pattern name id modifier extends implements Nothing Nothing
                   modifyState (p :)
               <?> "Complex Pattern"

-- | Simple patterns.
-- parsePatternS ::= <id> <- <modifier> Pattern(<name>) ;
parsePatternS :: Parser ()
parsePatternS = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens stringLiteral
                   let p = Pattern name id modifier Nothing Nothing Nothing Nothing
                   modifyState (p :)
               <?> "Simple Pattern"

-- ---------------------------------------------- [ Pattern Modifier Functions ]

-- | Parse the modifiers
-- parseModifier ::= parseModifierA | parseModifierI;
parseModifier :: Parser Modifier
parseModifier = try parseModifierA <|> parseModifierI <?> "Modifier"

parseModifierA :: Parser Modifier
parseModifierA = do reserved "Abstract"
                    return Abstract
                 <?> "Abstract"

parseModifierI :: Parser Modifier
parseModifierI = do reserved "Integration"
                    return Integration
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
                  ps <- getState
                  let exs = fmap (\id -> tryMkRelation id ps Nothing) ids
                  if Nothing `elem` exs
                     then error "Unknown identity used"
                     else return $ catMaybes exs
               <?> "Specialisation"


-- parseImplements ::= :implements <idlist>;
parseImplements :: Parser Realises
parseImplements = do reserved ":implements"                     
                     ids <- parseIDs
                     ps <- getState
                     let exs = fmap (\id -> tryMkRelation id ps Nothing) ids
                     if Nothing `elem` exs
                        then error "Unknown identity used"
                        else return $ catMaybes exs
                  <?> "Realisation"

-- ---------------------------------------------- [ Relation Parsing Functions ]

-- @TODO Make better!

-- | Parse a relation
-- parseRelation ::= relationM | relation1 ;
parseRelation :: Parser ()
parseRelation = try parseRelationM <|> parseRelation1 <?> "Relations"

-- ----------------------------------- [ Functions for 1-Many Relation Parsing ]

-- relationM ::= relationMu | relationMl
parseRelationM :: Parser ()
parseRelationM =  try parseRelationMu <|> parseRelationMl <?> "1-2-Many Relation" 

-- relationMu ::= <id> "uses" <idlist>;
parseRelationMu :: Parser ()
parseRelationMu = do from <- identifier
                     reserved "uses"
                     tos <- parseIDList
                     ps <- getState
                     if isNothing $ getPattern from ps
                        then error $ "From Pattern in linked relation doesn't exist: " ++ from
                        else do let rels = map (\to -> tryMkRelation to ps Nothing) tos                     -- Nasty Hack
                                if Nothing `elem` rels
                                   then error "Unknown Identity used in To relation of uses"
                                   else putState $ last $ map (\r -> addRequire from r ps) (catMaybes rels) -- Nasty Hack

-- relationMl ::= <id> "linkedTo" <idlist>;
parseRelationMl :: Parser ()
parseRelationMl = do from <- identifier
                     reserved "linkedTo"
                     tos <- parseIDList
                     ps <- getState
                     if isNothing $ getPattern from ps
                        then error $ "From Pattern in linked relation doesn't exist: " ++ from
                        else do let rels = map (\to -> tryMkRelation to ps Nothing) tos                     -- Nasty Hack
                                if Nothing `elem` rels
                                   then error "Unknown Identity used in To relation of linkedTo"
                                   else putState $ last $ map (\r -> addLink from r ps) (catMaybes rels)

-- -------------------------------------- [ Functions for 1-1 Relation Parsing ]

-- parseRelation1 ::= relation1u | relation1l ;
parseRelation1 :: Parser ()            
parseRelation1 = try parseRelation1u <|> parseRelation1l <?> "1-2-1 Relation with Description"

-- relation1u ::= <id> "uses" <id> parseRelDesc?                    
parseRelation1u :: Parser ()
parseRelation1u = do from <- identifier
                     reserved "uses"
                     to <- identifier
                     desc <- optionMaybe parseRelDesc
                     ps <- getState
                     if isNothing $ getPattern from ps
                        then error $ "From Pattern in used relation doesn't exist: " ++ from
                        else do let res = tryMkRelation to ps desc
                                if isNothing res
                                   then error $ "To Pattern in used relation doesn't exist: " ++ to
                                   else putState (addRequire from (fromJust res) ps)
                  <?> "1-2- Uses Relation with Description"


-- relation1l ::= <id> "linkedTo" <id> parseRelDesc?
parseRelation1l :: Parser ()
parseRelation1l = do from <- identifier
                     reserved "linkedTo"
                     to <- identifier
                     desc <- optionMaybe parseRelDesc
                     ps <- getState
                     if isNothing $ getPattern from ps
                        then error $ "From Pattern in linkedTO relation doesn't exist:  " ++ from
                        else do let res = tryMkRelation to ps desc
                                if isNothing res
                                   then error $ "To Pattern in linkedTo relation doesn't exist: " ++ to
                                   else putState (addLink from (fromJust res) ps)
                  <?> "1-2-1 LinkedTo Relation with Description"

-- -- -------------------------------------------------- [ Misc Parsing Functions ]

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
