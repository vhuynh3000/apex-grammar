lexer grammar ApexLexer;

@header {
}

// LEXER

SELECT : S E L E C T ;
FROM   : F R O M ;
WHERE  : W H E R E ;
GROUP_BY  : G R O U P ' '+ B Y ;
ORDER_BY  : O R D E R ' '+ B Y ;
HAVING : H A V I N G ;
ASC    : A S C ;
DESC   : D E S C ;
AND    : A N D ;
OR     : O R ;
NOT    : N O T ;
LIKE   : L I K E ;
IN     : I N ;
AS     : A S ;
USING  : U S I N G ;
FOR_UPDATE    : F O R ' '+ U P D A T E ;
FOR_VIEW      : F O R ' '+ V I E W ;
FOR_REFERENCE : F O R ' '+ R E F E R E N C E ;
UPDATE_TRACKING : U P D A T E ' '+ T R A C K I N G ;
UPDATE_VIEWSTAT : U P D A T E ' '+ V I E W S T A T ;

//TODO: NULLS is a keyword but first and last are not
NULLS_FIRST  : N U L L S ' '+ F I R S T;
NULLS_LAST  : N U L L S ' '+ L A S T;

INSERT : I N S E R T ;
UPDATE : U P D A T E ;
UPSERT : U P S E R T ;
DELETE : D E L E T E ;
UNDELETE : U N D E L E T E;

// §3.9 Keywords

ABSTRACT      : A B S T R A C T;
BOOLEAN       : B O O L E A N;
BREAK         : B R E A K;
BYTE          : B Y T E;
CASE          : C A S E;
CATCH         : C A T C H;
CHAR          : C H A R;
CLASS         : C L A S S;
CONST         : C O N S T;
CONTINUE      : C O N T I N U E;
DEFAULT       : D E F A U L T;
DO            : D O;
DOUBLE        : D O U B L E;
ELSE          : E L S E;
ENUM          : E N U M;
EXTENDS       : E X T E N D S;
FINAL         : F I N A L;
FINALLY       : F I N A L L Y;
FLOAT         : F L O A T;
FOR           : F O R;
IF            : I F;
GLOBAL        : G L O B A L;
GOTO          : G O T O;
IMPLEMENTS    : I M P L E M E N T S;
IMPORT        : I M P O R T;
INSTANCEOF    : I N S T A N C E O F;
INT           : I N T;
INTERFACE     : I N T E R F A C E;
LONG          : L O N G;
NEW           : N E W;
OVERRIDE      : O V E R R I D E;
PACKAGE       : P A C K A G E;
PRIVATE       : P R I V A T E;
PROTECTED     : P R O T E C T E D;
PUBLIC        : P U B L I C;
RETURN        : R E T U R N;
SHORT         : S H O R T;
STATIC        : S T A T I C;
SUPER         : S U P E R;
THIS          : T H I S;
THROW         : T H R O W;
TESTMETHOD    : T E S T M E T H O D;
TRY           : T R Y;
VOID          : V O I D;
WEBSERVICE    : W E B S E R V I C E;
WHILE         : W H I L E;
VIRTUAL       : V I R T U A L;

// §3.10.1 Integer Literals

IntegerLiteral
    :   DecimalIntegerLiteral
    |   HexIntegerLiteral
    |   OctalIntegerLiteral
    |   BinaryIntegerLiteral
    ;

fragment
DecimalIntegerLiteral
    :   DecimalNumeral IntegerTypeSuffix?
    ;

fragment
HexIntegerLiteral
    :   HexNumeral IntegerTypeSuffix?
    ;

fragment
OctalIntegerLiteral
    :   OctalNumeral IntegerTypeSuffix?
    ;

fragment
BinaryIntegerLiteral
    :   BinaryNumeral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   [lL]
    ;

fragment
DecimalNumeral
    :   '0'
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

// §3.10.2 Floating-Point Literals

FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;

// §3.10.3 Boolean Literals

BooleanLiteral
    :   TRUE
    |   FALSE
    ;

TRUE  : T R U E ;
FALSE : F A L S E;

fragment
SingleCharacter
    :   ~['\\]
    ;

// §3.10.5 String Literals

StringLiteral
    :   '\'' StringCharacters? '\''
    ;

fragment
StringCharacters
    :   StringCharacter+
    ;

fragment
StringCharacter
    :   ~['\\]
    |   EscapeSequence
    ;

// §3.10.6 Escape Sequences for Character and String Literals

fragment
EscapeSequence
    :   '\\' [btnfr"'\\]
    |   OctalEscape
    |   UnicodeEscape
    ;

fragment
OctalEscape
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' ZeroToThree OctalDigit OctalDigit
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment
ZeroToThree
    :   [0-3]
    ;

// §3.10.7 The Null Literal

NullLiteral
    :   N U L L
    ;

DateTimeLiteral
    :  Digit Digit Digit Digit '-' Digit Digit '-' Digit Digit T Digit Digit ':' Digit Digit ':' Digit Digit '.' Digit Digit Digit Z
    ;

// §3.11 Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';

// §3.12 Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
NOTEQUAL2       : '<>';
SHORT_AND       : '&&';
SHORT_OR        : '||';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';

ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';
LSHIFT_ASSIGN   : '<<=';
RSHIFT_ASSIGN   : '>>=';
URSHIFT_ASSIGN  : '>>>=';
MAP_ASSIGN      : '=>';

// §3.8 Identifiers (must appear after all keywords in the grammar)

Identifier
    :   JavaLetter JavaLetterOrDigit*
    ;

fragment
JavaLetter
    :   [a-zA-Z$_] // these are the "java letters" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierStart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

fragment
JavaLetterOrDigit
    :   [a-zA-Z0-9$_] // these are the "java letters or digits" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierPart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

//
// Additional symbols not defined in the lexical specification
//

AT : '@';
ELLIPSIS : '...';

//
// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;

// case insensitive fragments
fragment A: [aA] ;
fragment B: [bB] ;
fragment C: [cC] ;
fragment D: [dD] ;
fragment E: [eE] ;
fragment F: [fF] ;
fragment G: [gG] ;
fragment H: [hH] ;
fragment I: [iI] ;
fragment J: [jJ] ;
fragment K: [kK] ;
fragment L: [lL] ;
fragment M: [mM] ;
fragment N: [nN] ;
fragment O: [oO] ;
fragment P: [pP] ;
fragment Q: [qQ] ;
fragment R: [rR] ;
fragment S: [sS] ;
fragment T: [tT] ;
fragment U: [uU] ;
fragment V: [vV] ;
fragment W: [wW] ;
fragment X: [xX] ;
fragment Y: [yY] ;
fragment Z: [zZ] ;


