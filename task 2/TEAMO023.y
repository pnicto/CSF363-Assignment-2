%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
void yyerror();
%}

%token PROGRAM BEGIN END DOT SEMICOLON IDENTIFIER COMMA E COLON

%%
program: program_heading block DOT
       ;
program_heading: PROGRAM identifier SEMICOLON
               ;
block: declaration_part statement_part
     ;
declaration_part:
                | variable_declaration_part
                ;
statement_part: BEGIN statement_sequence END
              ;
variable_declaration_part: VAR variable_declaration SEMICOLON more_variable_declarations
                         ;
more_variable_declarations: variable_declaration SEMICOLON more_variable_declarations
              |
              ;
variable_declaration: identifier_list COLON type
                    ;
identifier_list: identifier COMMA identifier_list
               | identifier
               ;
statement_sequence: statement SEMICOLON statement_sequence
                  | statement
                  ;
%%

int main() {
  yyparse();
  return 0;
}

void yyerror() {
  printf("Syntax error\n");
}
