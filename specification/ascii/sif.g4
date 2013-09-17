// ------------------------------------------ [ An Antlr4 Grammar for Sif-lang ]
grammar sif;

plang
    : plangDecl imports* patterns+ relations* EOF
    ;

// -------------------------------------------- [ Pattern Language Declaration ]

plangDecl
    : LANGUAGE STRING AS ID
    ;

// ----------------------------------------------------------------- [ Imports ]
imports
    : FROM ID IMPORT (ID | idlist)
    | IMPORT ID
    ;

// ---------------------------------------------------------------- [ Patterns ]
patterns
    : ID OpASSIGNMENT modifier? patternType? PATTERN LPAREN STRING? RPAREN
    ;

modifier
    : ABSTRACT | CONCRETE
    ;

patternType
    : TyCOMPONENT | TySYSTEM | TyDEPLOYMENT | TyADMIN | TyIMPLEMENTATION
    ;

// ---------------------------------------------------------------- [ Relation ]

relations
    : ID relationType (ID (COLON STRING)? | LBRAC idlist RBRAC ) 
    ;

relationType
    : relationTypeOp | relationTypeTxt
    ;

relationTypeOp
    : OpAGGREGATION | OpASSOCIATION | OpSPECIALISATION | OpREALISATION
    ;

relationTypeTxt
    : RelAGGREGATION | RelASSOCIATION | RelSPECIALISATION | RelREALISATION
    ;

// ID lists
idlist
    : ID (COMMA ID)*
    ;


// ------------------------------------------------------------------- [ Lexer ]

// ------------------------------------------------------------------ [ Tokens ]
// Relations
RelASSOCIATION    : 'linkedTo'; 
RelAGGREGATION    : 'uses'; 
RelSPECIALISATION : 'extends'; 
RelREALISATION    : 'implements'; 

// Pattern Types
TyPATTERN        : 'Pattern';
TyCOMPONENT      : 'Component'; 
TySYSTEM         : 'System'; 
TyDEPLOYMENT     : 'Deployment'; 
TyADMIN          : 'Admin'; 
TyIMPLEMENTATION : 'Implementation'; 
// Modifiers
TyABSTRACT : 'Abstract';
TyCONCRETE : 'Integration';

// Misc
LANGUAGE  : 'language';
PATTERNS  : 'patterns';
RELATIONS : 'relations';
FROM      : 'from';
IMPORT    : 'import';
AS        : 'as';

// Operators
OpASSIGNMENT     : ':='; 
OpAGGREGATION    : 'o-'; 
OpASSOCIATION    : '->'; 
OpSPECIALISATION : '<-'; 
OpREALISATION    : '=>'; 

// Punctuation
COLON  : ':';
COMMA  : ',';
LSBRAC : '[';
RSBRAC : ']';
LPAREN : '(';
RPAREN : ')';

// ----------------------------------------------------------- [ String and ID ]

STRING
    : '"' ( '\\"' | . )*? '"'
    ;

ID
    : [a-zA-Z_] [a-zA-Z_0-9]*
    ;

// --------------------------------------------------------------------- [ EOF ]
