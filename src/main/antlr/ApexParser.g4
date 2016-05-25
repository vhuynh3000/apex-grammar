/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** 
 *  Grammar for Salesforce's Apex language. 
 *  Based on Terence Parr, Sam Harwell's Java 1.7 grammar for ANTLR v4 with 
 *  modifications for Apex specific features such as SOQL.
 *
 */
parser grammar ApexParser;

options { tokenVocab = ApexLexer; }

@header {

}

// starting point for parsing a java file
compilationUnit
    :   typeDeclaration* EOF
    ;

typeDeclaration
    :   classOrInterfaceModifier* classDeclaration
    |   classOrInterfaceModifier* interfaceDeclaration
    |   ';'
    ;

modifier
    :   classOrInterfaceModifier
    |   transientModifier
    ;

classOrInterfaceModifier
    :   annotation       // class or interface
    |   (   GLOBAL       // class or interface
        |   PUBLIC       // class or interface
        |   PROTECTED    // class or interface
        |   PRIVATE      // class or interface
        |   STATIC       // class or interface
        |   TESTMETHOD   // class or interface
        |   sharingModifier // class only
        |   WEBSERVICE   // class only
        |   VIRTUAL      // class only
        |   ABSTRACT     // class or interface
        |   FINAL        // class only -- does not apply to interfaces
        )
    ;

variableModifier
    :   FINAL
    |   annotation
    ;

sharingModifier
    :   {
            (_input.LT(1).getText().toLowerCase().matches("(with|without)")) &&
            (_input.LT(2).getText().toLowerCase().matches("(sharing)"))
        }? Identifier Identifier
    ;

transientModifier
    :   {(_input.LT(1).getText().toLowerCase().matches("(transient)"))}? Identifier
    ;

classDeclaration
    :   CLASS Identifier typeParameters?
        (EXTENDS type)?
        (IMPLEMENTS typeList)?
        classBody
    ;

typeParameters
    :   '<' typeParameter (',' typeParameter)* '>'
    ;

typeParameter
    :   Identifier?
    ;

enumDeclaration
    :   ENUM Identifier '{' enumConstants? '}'
    ;

enumConstants
    :   enumConstant (',' enumConstant)*
    ;

enumConstant
    :  Identifier
    ;

interfaceDeclaration
    :   INTERFACE Identifier typeParameters? (EXTENDS typeList)? interfaceBody
    ;

typeList
    :   type (',' type)*
    ;

classBody
    :   '{' classBodyDeclaration* '}'
    ;

interfaceBody
    :   '{' interfaceBodyDeclaration* '}'
    ;

classBodyDeclaration
    :   ';'
    |   STATIC? block
    |   modifier* memberDeclaration
    ;

memberDeclaration
    :   methodDeclaration
    |   fieldDeclaration
    |   propertyDeclaration
    |   constructorDeclaration
    |   interfaceDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;

methodDeclaration
    :   (OVERRIDE)? (type|VOID) Identifier formalParameters
        (throwsDeclaration)?
        (   methodBody
        |   ';'
        )
    ;

constructorDeclaration
    :   Identifier formalParameters (throwsDeclaration)?
        constructorBody
    ;

throwsDeclaration
    :   throwsToken qualifiedNameList
    ;

throwsToken
    :   {(_input.LT(1).getText().toLowerCase().matches("(throws)"))}? Identifier
    ;

fieldDeclaration
    :   type variableDeclarators ';'
    ;

propertyDeclaration
    :   type propertyDeclarator
    ;

interfaceBodyDeclaration
    :   modifier* interfaceMemberDeclaration
    |   ';'
    ;

interfaceMemberDeclaration
    :   constDeclaration
    |   interfaceMethodDeclaration
    |   interfaceDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;

constDeclaration
    :   type constantDeclarator (',' constantDeclarator)* ';'
    ;

constantDeclarator
    :   Identifier '=' variableInitializer
    ;

interfaceMethodDeclaration
    :   (type|VOID) Identifier formalParameters
        (throwsDeclaration)?
        ';'
    ;

variableDeclarators
    :   variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    :   variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    :   Identifier
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;

arrayInitializer
    :   '{' (variableInitializer (',' variableInitializer)* )? '}'
    ;

propertyDeclarator
    :   propertyDeclaratorId '{' (modifier* propertyGetSetToken (';'|block))? (modifier* propertyGetSetToken (';'|block))? '}'
    ;

propertyGetSetToken
    : {(_input.LT(1).getText().toLowerCase().matches("(get|set)"))}? Identifier
    ;

propertyDeclaratorId
    :   Identifier
    ;

enumConstantName
    :   Identifier
    ;

type
    :   classOrInterfaceType ('[' ']')*
    |   primitiveType ('[' ']')*
    ;

classOrInterfaceType
    :   Identifier typeArguments? ('.' Identifier typeArguments? )*
    ;

primitiveType
    :   BOOLEAN
    |   INT
    |   LONG
    |   DOUBLE
    ;

typeArguments
    :   '<' typeArgumentList '>'
    ;

typeArgumentList
    :   typeArgument (',' typeArgument)*
    ;

typeArgument
    :   type
    |   '?' ((EXTENDS | SUPER) type)?
    ;

qualifiedNameList
    :   qualifiedName (',' qualifiedName)*
    ;

formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   formalParameter (',' formalParameter)*
    ;

formalParameter
    :   variableModifier* type variableDeclaratorId
    ;

methodBody
    :   block
    ;

constructorBody
    :   block
    ;

qualifiedName
    :   Identifier ('.' Identifier)*
    ;

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   StringLiteral
    |   BooleanLiteral
    |   NullLiteral
    ;

// ANNOTATIONS

annotation
    :   '@' annotationName ( '(' ( elementValuePairs | elementValue )? ')' )?
    ;

annotationName : qualifiedName ;

elementValuePairs
    :   elementValuePair+
    ;

elementValuePair
    :   Identifier '=' elementValue
    ;

elementValue
    :   expression
    |   annotation
    |   elementValueArrayInitializer
    ;

elementValueArrayInitializer
    :   '{' (elementValue (',' elementValue)*)? (',')? '}'
    ;

// STATEMENTS / BLOCKS

block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   localVariableDeclarationStatement
    |   statement
    |   typeDeclaration
    ;

localVariableDeclarationStatement
    :    localVariableDeclaration ';'
    ;

localVariableDeclaration
    :   variableModifier* type variableDeclarators
    ;

statement
    :   block
    |   IF parExpression statement (ELSE statement)?
    |   FOR '(' forControl ')' statement
    |   WHILE parExpression statement
    |   DO statement WHILE parExpression ';'
    |   TRY block (catchClause+ finallyBlock? | finallyBlock)
    |   RETURN expression? ';'
    |   THROW expression ';'
    |   BREAK Identifier? ';'
    |   CONTINUE Identifier? ';'
    |   expression '(' expression ')' statement //TODO: the first expression should be limited to function calls
    |   ';'
    |   dmlStatement ';'
    |   statementExpression ';'
    |   Identifier ':' statement
    ;

catchClause
    :   CATCH '(' variableModifier* catchType Identifier ')' block
    ;

catchType
    :   qualifiedName ('|' qualifiedName)*
    ;

finallyBlock
    :   FINALLY block
    ;

forControl
    :   enhancedForControl
    |   forInit? ';' expression? ';' forUpdate?
    ;

forInit
    :   localVariableDeclaration
    |   expressionList
    ;

enhancedForControl
    :   variableModifier* type variableDeclaratorId ':' expression
    ;

forUpdate
    :   expressionList
    ;

dmlStatement
    :   dmlOperation expression
    ;

dmlOperation
    :   INSERT
    |   UPDATE
    |   UPSERT
    |   DELETE
    |   UNDELETE
    ;


// EXPRESSIONS

parExpression
    :   '(' expression ')'
    ;

expressionList
    :   expression (',' expression)*
    ;

statementExpression
    :   expression
    ;

constantExpression
    :   expression
    ;

expression
    :   primary
    |   '[' soql ']'
    |   expression '.' Identifier
    |   expression '.' THIS
    |   hackTriggerNew
    |   hackDatabaseDMLOperation
    |   expression '.' NEW nonWildcardTypeArguments? innerCreator
    |   expression '.' SUPER superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '(' expressionList? ')'
    |   NEW creator
    |   '(' type ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!') expression
    |   expression ('*'|'/'|'%') expression
    |   expression ('+'|'-') expression
    |   expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression INSTANCEOF type
    |   expression ('==' | '!=' | '<>') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression '&&' expression
    |   expression '||' expression
    |   expression '?' expression ':' expression
    |   <assoc=right> expression
        (   '='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '&='
        |   '|='
        |   '^='
        |   '>>='
        |   '>>>='
        |   '<<='
        |   '%='
        )
        expression
    ;

primary
    :   '(' expression ')'
    |   THIS
    |   SUPER
    |   literal
    |   Identifier
    |   type '.' CLASS
    |   VOID '.' CLASS
    |   nonWildcardTypeArguments (explicitGenericInvocationSuffix | THIS arguments)
    ;

hackTriggerNew
    :   hackTriggerClass '.' NEW
    ;

hackTriggerClass
    :   {(_input.LT(1).getText().toLowerCase().matches("trigger"))}? Identifier
    ;

hackDatabaseDMLOperation
    :   hackDatabaseClass '.' dmlOperation
    ;

hackDatabaseClass
    :   {(_input.LT(1).getText().toLowerCase().matches("database"))}? Identifier
    ;

creator
    :   nonWildcardTypeArguments createdName classCreatorRest
    |   createdName (arrayCreatorRest | listCreatorRest | mapCreatorRest | classCreatorRest)
    ;

createdName
    :   Identifier typeArgumentsOrDiamond? ('.' Identifier typeArgumentsOrDiamond?)*
    |   primitiveType
    ;

innerCreator
    :   Identifier nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    :   '['
        (   ']' arrayInitializer
        |   expression ']'
        )
    ;

listCreatorRest
    :   '{' (expression (',' expression)* )? '}'
    ;

mapCreatorRest
    :   '{' (mapKeyValueInitializer (',' mapKeyValueInitializer)* )? '}'
    ;

mapKeyValueInitializer
    :   expression '=>' expression
    ;

classCreatorRest
    :   arguments
    ;

explicitGenericInvocation
    :   nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

nonWildcardTypeArguments
    :   '<' typeList '>'
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    :   '<' '>'
    |   nonWildcardTypeArguments
    ;

superSuffix
    :   arguments
    |   '.' Identifier arguments?
    ;

explicitGenericInvocationSuffix
    :   SUPER superSuffix
    |   Identifier arguments
    ;

arguments
    :   '(' expressionList? ')'
    ;

// Soql grammar definitions

soql
    : soqlStatement
    ;

soqlStatement
    : SELECT soqlSelectClause FROM soqlObject (AS? soqlAlias)? (soqlUsingClause)? (soqlWhereClause)? (soqlWithClause)? (soqlGroupByClause (soqlHavingClause)?)? (soqlOrderByClause)? (soqlLimitClause)? (soqlOffsetClause)? (soqlQueryModifier)?
    ;

soqlSelectClause
    : soqlSelectClausePart (',' soqlSelectClausePart)*
    ;

soqlSelectClausePart
    : soqlField (soqlAlias)?
    | soqlFunctionExpression (soqlAlias)?
    | '(' soqlSelectSubQuery ')'
    ;

soqlFunctionExpression
    : soqlFunction '(' soqlField? ')'
    ;

soqlSelectSubQuery
    : SELECT soqlSelectClause FROM soqlObject (AS? soqlAlias)? (soqlWhereClause)? (soqlLimitClause (soqlOffsetClause)?)? (soqlOrderByClause)?
    ;

soqlUsingClause
    : USING soqlScopeToken Identifier
    ;

soqlWhereClause
    : WHERE soqlConditions
    ;

soqlWithClause
    : soqlWithToken soqlConditions
    ;

soqlConditions
    : soqlAndConditions
    | soqlOrConditions
    | soqlNotConditions
    | soqlCondition
    ;

soqlAndConditions
    : soqlCondition AND soqlCondition (AND soqlCondition)*
    ;

soqlOrConditions
    : soqlCondition OR soqlCondition (OR soqlCondition)*
    ;

soqlNotConditions
    : NOT soqlCondition
    ;

soqlCondition
    : '(' soqlConditions ')'
    | soqlFieldCondition
    | soqlSetCondition
    ;

soqlFieldCondition
    : (soqlField | soqlFunctionExpression) soqlOperator soqlValue
    ;

soqlSetCondition
    : (soqlField | soqlFunctionExpression) soqlEqualsOperator '(' (soqlValueList | soqlWhereSubQuery) ')'
    ;

soqlWhereSubQuery
    : SELECT soqlField FROM soqlObject (AS? soqlAlias)? (soqlWhereClause)? (soqlLimitClause (soqlOffsetClause)?)?
    ;

soqlGroupByClause
    : GROUP_BY soqlField (',' soqlField)*
    ;

soqlHavingClause
    : HAVING soqlConditions
    ;

soqlOrderByClause
    : ORDER_BY soqlOrderByField (',' soqlOrderByField)*
    ;

soqlOrderByField
    : soqlField (ASC|DESC)? (NULLS_FIRST|NULLS_LAST)?
    ;

soqlLimitClause
    : soqlLimitToken soqlValue
    ;

//the token 'SCOPE' is not a keyword
soqlScopeToken
    : {(_input.LT(1).getText().toLowerCase().matches("(scope)"))}? Identifier
    ;

//the token 'LIMIT' is not a keyword
soqlLimitToken
    : {(_input.LT(1).getText().toLowerCase().matches("(limit)"))}? Identifier
    ;

//the token 'WITH' is not a keyword
soqlWithToken
    : {(_input.LT(1).getText().toLowerCase().matches("(with)"))}? Identifier
    ;

soqlOffsetClause
    : soqlOffsetToken soqlValue
    ;

//the token 'OFFSET' is not a keyword
soqlOffsetToken
    : {(_input.LT(1).getText().toLowerCase().matches("(offset)"))}? Identifier
    ;

soqlQueryModifier
    : FOR_UPDATE
    | FOR_VIEW
    | FOR_REFERENCE
    | UPDATE_TRACKING
    | UPDATE_VIEWSTAT
    ;

soqlOperator
    : soqlEqualsOperator
    | '<' 
    | '<=' 
    | '>' 
    | '>=' 
    | (NOT)? LIKE
    ;

soqlEqualsOperator
    : '=' 
    | '!=' 
    | '<>' 
    | (NOT)? IN
    ;

soqlValue
    : literal
    | soqlDateTime
    | soqlTodayToken
    | ':' expression
    ;

soqlValueList
    : soqlValue (',' soqlValue)*
    ;

soqlTodayToken
    : {(_input.LT(1).getText().toLowerCase().matches("(today)"))}? Identifier
    ;

soqlIntegerValue
    : IntegerLiteral
    ;

soqlField
    : Identifier ('.' Identifier)*
    ;

soqlFunction
    : Identifier
    ;

soqlAlias
    : Identifier
    ;

soqlObject
    : Identifier
    ;

soqlDateTime
    : DateTimeLiteral
    ;

