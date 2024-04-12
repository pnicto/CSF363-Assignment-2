%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
void yyerror();
%}

%token PROGRAM BEGIN END DOT SEMICOLON COMMA COLON ASSIGNMENT READ WRITE LPAREN RPAREN WHILE DO FOR TO DOWNTO IF THEN ELSE PLUS MINUS MULTIPLY DIVIDE REMAINDER AND OR EQUAL NOT_EQUAL LESS LESS_OR_EQUAL GREATER GREATER_OR_EQUAL LSQUAREPAREN RSQUAREPAREN

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
structured_statement: compound_statement
                    | repetitive_statement
                    | if_statement
                    ;
compound_statement: BEGIN statement_sequence END
                  ;
repetitive_statement: while_statement
                    | for_statement
                    ;
while_statement: WHILE expression DO statement
               ;
for_statement: FOR variable_identifer ASSIGNMENT expression TO expression DO statement
             | FOR variable_identifer ASSIGNMENT expression DOWNTO expression DO statement
             ;
if_statement: IF expression THEN statement
            | IF expression THEN statement ELSE statement
            ;
expression: simple_expression relational_operator simple_expression
          | simple_expression
          ;
simple_expression: term additional_terms
                 | sign term additional_terms
                ;
additional_terms: addition_operator term additional_terms
                |
                ;
addition_operator: PLUS
                 | MINUS
                 | OR
                 ;
term: factor additional_factors
    ;
additional_factors: multiplication_operator factor additional_factors
                  |
                  ;
multiplication_operator: MULTIPLY
                       | DIVIDE
                       | REMAINDER
                       | AND
                       ;
relational_operator: EQUAL
                   | NOT_EQUAL
                   | LESS
                   | LESS_OR_EQUAL
                   | GREATER
                   | GREATER_OR_EQUAL
                   ;
factor: variable
      | number
      | string
      | LPAREN expression RPAREN
      | NOT factor
      ;
variable: identifier
        | indexed_variable
        ;
indexed_variable: array_variable LSQUAREPAREN expression_list RSQUAREPAREN
                ;
%%

int main() {
  yyparse();
  return 0;
}

void yyerror() {
  printf("Syntax error\n");
}
