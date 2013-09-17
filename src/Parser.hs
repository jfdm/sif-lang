-- | The parser.
module Parser (parseSif) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import Lexer
import Keywords
import AST

-- ------------------------------------------- [ Pattern Language Model Parser ]

-- | Parses a Sif Spec file into the corresponding AST
parseSif :: String -> PlangAST
parseSif fname =
    case runParser (runLex parsePlang) [] "" fname of
      Left err -> error (show err)
      Right ast -> ast

-- | Definition for a pattern language
-- parsePlang ::= parseMetadata parseImports? parsePattern+ parseRelation*;
parsePlang :: Parser PlangAST
parsePlang = do (title, label) <- parseMetadata
                optionMaybe parseImports
                reserved sifKWordPattern
                many parsePattern
                reserved sifKWordRelation
                relations <- manyTill parseRelations eof
                pState <- getState
                return (PlangAST title label (reverse pState) ((reverse . concat) relations))
             <?> "Language Instance"

-- ---------------------------------------------------- [ Language Declaration ]

-- | Definition for a pattern language metadata.
-- parseLangDecl ::= language <title> as <id>
parseMetadata :: Parser (String, ID)
parseMetadata = do reserved sifKWordLang
                   title <- stringLiteral
                   reserved sifKWordAs
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
parseImportM = do reserved sifKWordFrom
                  lang <- identifier
                  reserved sifKWordImport
                  pIDs <- parseIDList
                  return $ map (`mkImportPattern` lang) pIDs
               <?> "Many Imports"

-- | Import a pattern language
-- parseImportLang ::= import <lang>;
parseImportLang :: Parser PatternsExpr
parseImportLang = do reserved sifKWordImport
                     lang <- identifier
                     return [mkImportPattern lang lang]
                  <?> "Language Import"

-- ----------------------------------------------- [ Pattern Parsing Functions ]

-- | Pattern Definition
-- parsePattern ::= <id> := parseModifier? parseType? Pattern(<title>?) ;
parsePattern :: Parser PatternExpr
parsePattern = do ident <- identifier
                  reservedOp sifOpAssignment
                  mod <- optionMaybe parsePatternModifier
                  typ <- optionMaybe parsePatternType
                  reserved sifKWordTypPat
                  name <- parens $ optionMaybe stringLiteral
                  let p = mkPattern ident name (fromMaybe TyPattern typ) mod
                  modifyState (p: )
                  return p

-- --------------------------------------------------- [ Pattern Type Modifier ]

-- | Parse the modifiers
-- parseModifier ::= ( "Abstract" | "Concrete");
parsePatternModifier :: Parser TyModifier
parsePatternModifier = do try $ reserved sifKWordTypModAbs
                          return TyModAbstract
                   <|> do reserved sifKWordTypModConc
                          return TyModConcrete
                   <?> "Type Modifier"

-- ---------------------------------------------------- [ Pattern Type Parsing ]
-- | Parse the type information
-- parsePatternType ::= ("Component" | "System" | "Deployment"
--                         | "Admin" | "Implementation");
parsePatternType :: Parser TyPattern
parsePatternType = do try $ reserved sifKWordTypComp
                      return TyComponent
               <|> do reserved sifKWordTypSys
                      return TySystem
               <|> do reserved sifKWordTypDeplo
                      return TyDeployment
               <|> do reserved sifKWordTypAdmin
                      return TyAdmin
               <|> do reserved sifKWordTypImpl
                      return TyImplementation
               <?> "Pattern Type"

-- ---------------------------------------------- [ Relation Parsing Functions ]

-- | Parse a relation
-- parseRelation ::= parseRelationM | parseRelation1 ;
parseRelations :: Parser RelationsExpr
parseRelations = try parseRelationM <|> parseRelation1 <?> "Relations"

-- ----------------------------------- [ Functions for 1-Many Relation Parsing ]

-- | Parse a 1-2-Many Relation
-- parserelationM ::= parseRelationFrom parseIDList;
parseRelationM :: Parser RelationsExpr
parseRelationM = do (from, typ) <- parseRelationFrom 
                    tos <- parseIDList
                    ps <- getState
                    let rs = if any (\p -> notElem (ident p) tos) ps
                             then fail $ "Unknown Identity used"
                             else map (\to -> mkRelation from to typ Nothing) tos
                    return rs
              <?> "1-2-Many Relation"


-- -------------------------------------- [ Functions for 1-1 Relation Parsing ]

-- | Parse a 1-2-1 Relation with optional description.
-- parseRelation1 ::= parseRelationFrom <id> parseRelDesc?
parseRelation1 :: Parser RelationsExpr            
parseRelation1 = do (from, typ) <- parseRelationFrom
                    to <- identifier
                    desc <- optionMaybe parseRelationDesc
                    ps <- getState
                    let toP = getPattern to ps
                    if isNothing toP
                    then fail $ "To Pattern in relation doesn't exist: " ++ to
                    else return [mkRelation from to typ desc]                 
                  <?> "1-2-1 Relation with Description"

-- ------------------------------------------------- [ Relation Util Functions ]

-- | Parse the from and type of relation.
-- parseRelationFrom ::= <id> parseRelationType ;
parseRelationFrom :: Parser (ID, TyRelation)
parseRelationFrom = do from <- identifier
                       typ <- parseRelationType
                       ps <- getState
                       let fromP = getPattern from ps
                       if isNothing fromP
                       then fail $ "From Pattern in relation doesn't exist: " ++ from
                       else return (from, typ)
                    <?> "From Relation"

-- | Parse the relation type.
-- parseRelationType ::= parseRelationTypeOp | parseRelationTypeTxt ;
parseRelationType :: Parser TyRelation
parseRelationType = try parseRelationTypeOp <|> parseRelationTypeTxt <?> "Relation Type"

-- | Parse the operator indicator for a relation type.
-- parseRelationTypeOp ::= ("->" | "o-" | "<-" | "=>" );
parseRelationTypeOp :: Parser TyRelation
parseRelationTypeOp = do try $ reservedOp sifOpAssociation
                         return TyAssociation
                  <|> do reservedOp sifOpAggregation
                         return TyAggregation
                  <|> do reservedOp sifOpSpecialisation
                         return TySpecialisation
                  <|> do reservedOp sifOpRealisation
                         return TyRealisation
                  <?> "Relation Type Operator"

-- | Parse the textual indicator for a relation type.
-- parseRelationTypeTxt ::= ("linkedTo" | "uses" | "extends" | "implements");
parseRelationTypeTxt :: Parser TyRelation
parseRelationTypeTxt = do try $ reserved sifKWordAssociation
                          return TyAssociation
                   <|> do reserved sifKWordAggregation
                          return TyAggregation
                   <|> do reserved sifKWordSpecialisation
                          return TySpecialisation
                   <|> do reserved sifKWordRealisation
                          return TyRealisation
                   <?> "Relation Type Textual"

-- | Parse a description.
-- parseRelationDesc ::= ":" <desc>
parseRelationDesc :: Parser String
parseRelationDesc = do reservedOp sifOpDescription
                       stringLiteral
                    <?> "Relation Description"

-- ---------------------------------------------------- [ ID Parsing Functions ]

-- | Parse a list of IDs
-- parseIDs ::= parseID1 | parseIDList;
parseIDs :: Parser IDs
parseIDs = try parseIDListB <|> parseID1 <?> "ID Lists"

-- | Parse a list of IDs delineated by brackets.
-- parseIDListB ::= [ parseIDList ];
parseIDListB :: Parser IDs
parseIDListB = brackets parseIDList

-- | Parse a csv list of IDs
-- parseIDList ::= <id> (',' <id>)* ;
parseIDList :: Parser IDs
parseIDList = sepBy1 identifier comma

-- | Parse a single ID
-- parseID1 ::= <id>;
parseID1 :: Parser IDs
parseID1 = do id <- identifier
              return [id]

-- -- --------------------------------------------------------------------- [ EOF ]
