// ------------------------------------------ [ An Antlr4 Grammar for Sif-lang ]
grammar sif;

plang
    : plangDecl imports* expr+ EOF
    ;

// -------------------------------------------- [ Pattern Language Declaration ]

plangDecl
    : PLANG STRING AS ID
    ;

// ----------------------------------------------------------------- [ Imports ]
imports
    : FROM ID IMPORT (ID | identlist)
    | IMPORT ID
    ;

// ------------------------------------------------------------- [ Expressions ]
expr
    : PAT patternDecl+  REL relation+
    ;

// ---------------------------------------------------- [ Pattern Declarations ]

patternDecl
    : ID ASSIGNMENT modifier? PATTERN LPAREN STRING? RPAREN
    | ID ASSIGNMENT PATTERN LPAREN STRING RPAREN LCBRAC property+ RCBRAC
    ;

property
    :  ( SPECIALISATION | REALISATION ) ( identlist )
    ;

modifier
    : ABSTRACT | INTEGRATION
    ;

// --------------------------------------------------- [ Relation Declarations ]

relation
    : ID ( instanceRelation ) (ID (SEP STRING)? | identlist ) 
    ;

instanceRelation
    : ASSOCIATION
    | AGGREGATION 
    ;

// ID lists

identlist
    : LSBRAC ID (COMMA ID)* RSBRAC
    ;


// ------------------------------------------------------------------- [ Lexer ]

// ------------------------------------------------------------------ [ Tokens ]
//  Relations
ASSOCIATION    : 'linkedTo';
AGGREGATION    : 'uses';
// Properties
TYPEPROP       : ':ofType';
SPECIALISATION : ':extends';
REALISATION    : ':implements';
// Types
PATTERN        : 'Pattern';
ABSTRACT       : 'Abstract';
INTEGRATION    : 'Integration';
PLANG          : 'language'
// Misc
PAT            : 'patterns';
REL            : 'relations';
FROM           : 'from';
IMPORT         : 'import';
AS             : 'as';
SEP            : ':';
ASSIGNMENT     : '<-';
COMMA          : ',';
LCBRAC         : '{';
RCBRAC         : '}';
LSBRAC         : '[';
RSBRAC         : ']';
LPAREN         : '(';
RPAREN         : ')';

// ----------------------------------------------------------- [ String and ID ]

STRING
    : '"' ( '\\"' | . )*? '"'
    ;

ID
    : [a-zA-Z_] [a-zA-Z_0-9]*
    ;


// ------------------------------------------------------------- [ White Space ]

COMMENT
    :   '--' ~[\r\n]* -> skip
    ;

WS
	: ( ' '
	| '\t'
	| '\n'
	| '\r'
	)+ -> channel(HIDDEN)
;


// --------------------------------------------------------------------- [ EOF ]
