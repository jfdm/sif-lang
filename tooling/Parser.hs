module Parser (parsePatternLang) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.String (parseFromFile)
import Lexer
import Model

-- -------------------------------------------------- [ Misc Parsing Functions ]

-- parseID1 ::= <id>;
parseID1 :: Parser IDs
parseID1 = do id <- identifier
              return (id : [])

-- parseIDList ::= [ <id> (<id> ',')* ];
parseIDList :: Parser IDs
parseIDList = do ids <- brackets $ sepBy1 identifier comma
                 return ids

-- parseIDs ::= <id> | <idlist>;
parseIDs :: Parser IDs
parseIDs = try parseIDList <|> parseID1 <?> "ID Lists"


-- ---------------------------------------------- [ Relation Parsing Functions ]
-- @TODO Simplify?
{-

relationM ::= <id> "uses" <idlist>
           | <id> "linkedTo" <idlist>}
           ;

-}
parseRelationM :: Parser Relation
parseRelationM = do from <- identifier
                    reserved "uses"
                    to <- parseIDs
                    return (Requires from to Nothing)
             <|> do from <- identifier
                    reserved "linkedTo"
                    to <- parseIDs
                    return (Links from to Nothing)
             <?> "1-2-Many Relation"

{-

relationM ::= <id> "uses" <id> ":" <desc>
           | <id> "linkedTo" <id> ":" <desc>
           ;

-}                         
parseRelation1 :: Parser Relation
parseRelation1 = do from <- identifier
                    reserved "uses"
                    to <- parseID1
                    reservedOp ":"
                    desc <- optionMaybe stringLiteral
                    return (Requires from to desc)
             <|> do from <- identifier
                    reserved "linkedTo"
                    to <- parseID1
                    reservedOp ":"
                    desc <- optionMaybe stringLiteral
                    return (Links from to desc)
             <?> "1-2-1 Relation with Description"

-- parseRelation ::= relationM | relation1 ;
parseRelation :: Parser Relation
parseRelation = try parseRelation1 <|> parseRelationM <?> "Relations"

-- ----------------------------------------------- [ Pattern Parsing Functions ]

parseModifierA :: Parser Modifier
parseModifierA = do reserved "Abstract"
                    return "Abstract"

parseModifierI :: Parser Modifier
parseModifierI = do reserved "Abstract"
                    return "Integration"

-- parseModifier ::= parseModifierA | parseModifierI;
parseModifier :: Parser Modifier
parseModifier = try parseModifierA
                <|> parseModifierI
                <?> "Modifier"

-- parsePatternS ::= <id> <- <modifier> Pattern(<name>) ;
parsePatternS :: Parser Pattern
parsePatternS = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens $ stringLiteral
                   return (Pattern name id Nothing Nothing modifier)
               <?> "Simple Pattern"

-- -------------------------------------------------------- [ Complex Patterns ]

-- parseExtends ::= :extends <idlist>;
parseExtends :: Parser IDs
parseExtends = do reserved ":extends"
                  ids <- parseIDs
                  return ids

-- parseImplements ::= :implements <idlist>;
parseImplements :: Parser IDs
parseImplements = do reserved ":implements"
                     ids <- parseIDs
                     return ids

-- parseProperty ::= parseImplements | parseExtends;
parseProperties :: Parser (Maybe IDs, Maybe IDs)
parseProperties = do extends <- optionMaybe parseExtends
                     implements <- optionMaybe parseImplements
                     return (extends, implements)
                 <?> "Properties"

-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser Pattern
parsePatternC = do id <- identifier
                   reservedOp "<-"
                   modifier <- optionMaybe parseModifier
                   reserved "Pattern"
                   name <- parens $ stringLiteral
                   (extends, implements) <- braces $ parseProperties
                   return (Pattern name id extends implements modifier)
               <?> "Complex Pattern"


-- parsePattern := parsePatternSimple | parsePatternC
parsePattern :: Parser Pattern
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"

-- ------------------------------------------------- [ Import Parsing Function ]

-- imports are a dirty hack need to imporve.

-- parseImportM ::= from <langID> import <idlist>;

parseImportM :: Parser Imports
parseImportM = do reserved "from"
                  lang <- identifier
                  reserved "import"
                  ps <- sepBy1 identifier comma
                  return $ (map (\x -> (Import lang (Just x) Nothing)) ps)

-- parseImportAlias ::= from <langID> import <id> as <id>;

parseImportAlias :: Parser Imports
parseImportAlias = do reserved "from"
                      langID <- identifier
                      reserved "import"
                      origiD <- identifier
                      reserved "as"
                      newID <- identifier
                      return ((Import langID (Just origiD) (Just newID)) : [] )

parseImport :: Parser Imports
parseImport = try parseImportAlias <|> parseImportM

-- parseImportGlobal ::= import <langID>;

parseImportGlobal :: Parser Imports
parseImportGlobal = do reserved "import"
                       langID <- identifier
                       return ((Import langID Nothing Nothing) : [])

parseImports :: Parser Imports
parseImports = do is <- many1 $ choice [parseImport,
                                        parseImportGlobal]
                  return $ concat is
              <?> "Imports"

-- ---------------------------------------------------- [ Language Declaration ]

-- parseLangDecl ::= language <title> as <id>
parseLangDecl :: Parser PLangLabel
parseLangDecl = do reserved "language"
                   title <- stringLiteral
                   reserved "as"
                   id <- identifier
                   return (PLangLabel title id)
                <?> "Language Declaration"

-- ------------------------------------------- [ Pattern Language Model Parser ]

parsePatternLang :: Parser PatternLang
parsePatternLang = do info <- parseLangDecl
                      imports <- optionMaybe parseImports
                      reserved "patterns"
                      patterns <- manyTill parsePattern (reserved "relations")
                      relations <- many1 parseRelation
                      return (PatternLang info imports patterns relations)
                   <?> "Language Instance"

-- ------------------------------------------------- [ Helper Testing Function ]

-- Test some parseExpression p with a given string s
testParseStr :: Show a => Parser a -> String -> IO ()
testParseStr p s =
    case (parse (runLex p) "" s) of
      Left err -> error (show err)
      Right x -> print x

-- Test some parseExpression p with a given file f
testParseFile :: Show a => Parser a -> String -> IO ()
testParseFile p f = do { res <- parseFromFile (runLex p) f
                       ; case res of
                           Left err -> error (show err)
                           Right x -> print x
                       }


-- --------------------------------------------------------------------- [ EOF ]
