%{
#include "y.tab.h"
%}

WHITESPACE \n|\t|" "

%x STRING

%%
 /* program tokens */
{WHITESPACE}*(?i:program) { return PROGRAM; }
{WHITESPACE}*(?i:begin) { return BEGINNING; }
{WHITESPACE}*(?i:end) { return END; }
{WHITESPACE}*(?i:var) { return VAR; }
 
 /* type tokens */
{WHITESPACE}*":" { return COLON; }
{WHITESPACE}*(?i:array) { return ARRAY; }
{WHITESPACE}*(?i:of) { return OF; }
{WHITESPACE}*".." { return DOTDOT; }
{WHITESPACE}*(?i:real) { return REAL; }
{WHITESPACE}*(?i:integer) { return INTEGER; }
{WHITESPACE}*(?i:char) { return CHAR; }
{WHITESPACE}*(?i:boolean) { return BOOLEAN; }

 /* statement tokens */
{WHITESPACE}*":=" { return ASSIGNMENT; }
{WHITESPACE}*(?i:read) { return READ; }
{WHITESPACE}*(?i:write) { return WRITE; }
{WHITESPACE}*(?i:while) { return WHILE; }
{WHITESPACE}*(?i:do) { return DO; }
{WHITESPACE}*(?i:for) { return FOR; }
{WHITESPACE}*(?i:to) { return TO; }
{WHITESPACE}*(?i:downto) { return DOWNTO; }
{WHITESPACE}*(?i:if) { return IF; }
{WHITESPACE}*(?i:then) { return THEN; }
{WHITESPACE}*(?i:else) { return ELSE; }

 /* expression tokens */
{WHITESPACE}*"*" { return MULTIPLY; }
{WHITESPACE}*"/" { return DIVIDE; }
{WHITESPACE}*"%" { return REMAINDER; }
{WHITESPACE}*(?i:and) { return AND; }
{WHITESPACE}*(?i:or) { return OR; }
{WHITESPACE}*(?i:not) { return NOT; }
{WHITESPACE}*"=" { return EQUAL; }
{WHITESPACE}*"<>" { return NOT_EQUAL; }
{WHITESPACE}*"<" { return LESS; }
{WHITESPACE}*"<=" { return LESS_OR_EQUAL; }
{WHITESPACE}*">" { return GREATER; }
{WHITESPACE}*">=" { return GREATER_OR_EQUAL; }

 /* low level definitions */
{WHITESPACE}*"+" { return PLUS; }
{WHITESPACE}*"-" { return MINUS; }
{WHITESPACE}*(?i:e) { return E; }
{WHITESPACE}*\" { BEGIN STRING; return DQUOTE; }
<STRING>{WHITESPACE}*"''" { return ESCAPE_SEQUENCE; }
 /* we also don't match \n in strings */
<STRING>{WHITESPACE}*[^"\n] { return ANY_CHARACTER_EXCEPT_QUOTE; }
<STRING>{WHITESPACE}*\" { BEGIN INITIAL; return DQUOTE; }
{WHITESPACE}*[0-9] { return DIGIT; }

 /* misc tokens */
{WHITESPACE}*"." { return DOT; }
{WHITESPACE}*";" { return SEMICOLON; }
{WHITESPACE}*"," { return COMMA; }
{WHITESPACE}*"(" { return LPAREN; }
{WHITESPACE}*")" { return RPAREN; }
{WHITESPACE}*"[" { return LSQUAREPAREN; }
{WHITESPACE}*"]" { return RSQUAREPAREN; }

{WHITESPACE}*[A-Za-z][a-zA-Z0-9_]* { return IDENTIFIER; }

 /* catch errors in all states */
<*>.|\n { return ERROR; }
%%
