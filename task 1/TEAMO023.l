%{
    #include <stdio.h>
    #include <string.h>
    #include <ctype.h>

    int lineNumber = 1;

    struct ValidToken {
        int lineNumber;
        char lexeme[100];
        char tokenType[100];
    };

    struct ValidToken tokens[1000];

    int currentIndex = 0;

    void addNewToken(char *lexeme, char *tokenType);
    void trimSpaces(char *s);
%}

WHITESPACE \t|" "

%x STRING WRITE WRITESTRING

%%
 /* program tokens */
{WHITESPACE}*(?i:program) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:begin) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:end) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:var) { addNewToken(yytext, "Keyword"); }
 
 /* type tokens */
{WHITESPACE}*":" { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*(?i:array) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:of) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*".." { addNewToken(yytext, "Operator"); }
{WHITESPACE}*(?i:real) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:integer) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:char) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:boolean) { addNewToken(yytext, "Keyword"); }

 /* statement tokens */
{WHITESPACE}*":=" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*(?i:read) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:write) { BEGIN WRITE; addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:while) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:do) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:for) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:to) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:downto) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:if) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:then) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:else) { addNewToken(yytext, "Keyword"); }

 /* expression tokens */
{WHITESPACE}*"*" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"/" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"%" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*(?i:and) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:or) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*(?i:not) { addNewToken(yytext, "Keyword"); }
{WHITESPACE}*"=" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"<>" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"<" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"<=" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*">" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*">=" { addNewToken(yytext, "Operator"); }

 /* low level definitions */
{WHITESPACE}*"+" { addNewToken(yytext, "Operator"); }
{WHITESPACE}*"-" { addNewToken(yytext, "Operator"); }
 /* Current implementation for strings also stores the double quotes" */
{WHITESPACE}*\" { BEGIN STRING; yymore(); }
 /* we also don't match \n in strings */
<STRING>{WHITESPACE}*[^"\n] { yymore(); }
<STRING>{WHITESPACE}*\" { BEGIN INITIAL; trimSpaces(yytext); if(strlen(yytext) > 3) { addNewToken(yytext, "String"); } else { addNewToken(yytext, "Character"); } }
 /* Implementation of strings for write */
<WRITE>{WHITESPACE}*\" { BEGIN WRITESTRING; yymore(); }
 /* we also don't match \n in strings */
<WRITESTRING>{WHITESPACE}*[^"\n] { yymore(); }
<WRITESTRING>{WHITESPACE}*\" { BEGIN WRITE; addNewToken(yytext, "String"); }
{WHITESPACE}*[0-9]+([.][0-9]+)?((?i:e)[+-]?[0-9]+([.][0-9]+)?)? { addNewToken(yytext, "Number"); }

 /* misc tokens */
{WHITESPACE}*"." { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*";" { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*"," { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*"(" { addNewToken(yytext, "Punctuator"); }
<WRITE>{WHITESPACE}*"(" { addNewToken(yytext, "Punctuator"); }
<WRITE>{WHITESPACE}*")" { BEGIN INITIAL; addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*")" { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*"[" { addNewToken(yytext, "Punctuator"); }
{WHITESPACE}*"]" { addNewToken(yytext, "Punctuator"); }

{WHITESPACE}*[A-Za-z][a-zA-Z0-9_]* { addNewToken(yytext, "Identifier"); }
<WRITE>{WHITESPACE}*[A-Za-z][a-zA-Z0-9_]* { addNewToken(yytext, "Identifier"); }

 /* Deal with single line comments */
{WHITESPACE}*"//".* { /* Do nothing */ }

 /* Deal with new lines */
{WHITESPACE}*\n { lineNumber++; } 

 /* catch errors in all states */
<*>.|\n { addNewToken(yytext, "Unrecognized Token"); }
%%

void addNewToken(char *lexeme, char *tokenType) {
    tokens[currentIndex].lineNumber = lineNumber;
    strcpy(tokens[currentIndex].lexeme, lexeme);
    strcpy(tokens[currentIndex].tokenType, tokenType);

    trimSpaces(tokens[currentIndex].lexeme);

    currentIndex++;
}

void trimSpaces(char *s) {
    char *p = s;
    int l = strlen(p);

    while (isspace(p[l - 1])) {
        p[--l] = 0;
    }
    while (*p && isspace(*p)) {
        ++p;
        --l;
    }

    memmove(s, p, l + 1);
} 

int main(int argc, char *argv[]) {
    char *filename = argv[1];
    yyin = fopen(filename, "r");

    yylex();

    printf("|===============================================================================|\n");
    printf("| Line Number | Lexeme                                   | Token Type           |\n");
    printf("|===============================================================================|\n");

    for (int i = 0; i < currentIndex; i++) {
        printf("| %-11d | %-40s | %-20s |\n",
            tokens[i].lineNumber,
            tokens[i].lexeme,
            tokens[i].tokenType
        );
    }
    printf("|===============================================================================|\n");
}
