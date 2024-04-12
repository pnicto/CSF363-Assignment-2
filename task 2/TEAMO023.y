%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
void yyerror();
%}

%token PROGRAM BEGIN END DOT SEMICOLON IDENTIFIER COMMA E COLON ASSIGNMENT READ WRITE LPAREN RPAREN

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
statement_sequence: statement SEMICOLON statement
                  | statement
                  ;
statement: simple_statement
         | structured_statement
         ;
simple_statement: assignment_statement
                | procedure_statement
                ;
assignment_statement: variable ASSIGNMENT expression
                    ;
procedure_statement: READ actual_parameter_list
                   | WRITE actual_parameter_list
                   ;
actual_parameter_list: LPAREN actual_parameter other_actual_parameters RPAREN
                     ;
other_actual_parameters: COMMA actual_parameter other_actual_parameters
                       |
                       ;
actual_parameter: actual_value
                | actual_variable
                ;
actual_value: expression
            ;
actual_variable: variable
               ;
%%

int main() {
  yyparse();
  return 0;
}

void yyerror() {
  printf("Syntax error\n");
}
