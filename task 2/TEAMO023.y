%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern FILE *yyin;
int yylex();
void yyerror();
%}

// program tokens
%token PROGRAM BEGINNING END VAR

// type tokens
%token COLON ARRAY OF DOTDOT REAL INTEGER CHAR BOOLEAN

// statement tokens
%token ASSIGNMENT READ WRITE WHILE DO FOR TO DOWNTO IF THEN ELSE

// expression tokens
%token MULTIPLY DIVIDE REMAINDER AND OR NOT EQUAL NOT_EQUAL LESS LESS_OR_EQUAL GREATER GREATER_OR_EQUAL

// low level definitions
// E = "E" | "e"
// ESCAPE_SEQUENCE = '' (two single quotes)
%token PLUS MINUS E DQUOTE ANY_CHARACTER_EXCEPT_QUOTE DIGIT

// misc tokens (includes tokens that are shared by different groups)
%token DOT SEMICOLON COMMA LPAREN RPAREN LSQUAREPAREN RSQUAREPAREN IDENTIFIER ERROR


// Precedence rule for else shift-reduce conflict
%right THEN ELSE

%%
program: program_heading block DOT { printf("valid input\n"); return 0; }
       ;
program_heading: PROGRAM IDENTIFIER SEMICOLON
               ;
block: declaration_part statement_part
     ;
declaration_part:
                | variable_declaration_part
                ;
statement_part: BEGINNING statement_sequence END
              ;
variable_declaration_part: VAR variable_declaration SEMICOLON more_variable_declarations
                         ;
more_variable_declarations: variable_declaration SEMICOLON more_variable_declarations
                          |
                          ;
variable_declaration: identifier_list COLON type
                    ;
identifier_list: IDENTIFIER COMMA identifier_list
               | IDENTIFIER
               ;
type: REAL
    | INTEGER
    | CHAR
    | BOOLEAN
    | array_type
    ;
array_type: ARRAY LSQUAREPAREN subrange_type RSQUAREPAREN OF type
          ;
subrange_type: constant DOTDOT constant
             ;
statement_sequence: statement SEMICOLON statement_sequence
                  | statement
                  |
                  ;
statement: simple_statement
         | structured_statement
         ;
simple_statement: assignment_statement
                | procedure_statement
                ;
assignment_statement: variable ASSIGNMENT expression
                    | variable ASSIGNMENT char
                    ;
procedure_statement: READ LPAREN variable RPAREN
                   | WRITE actual_parameter_list
                   ;
actual_parameter_list: LPAREN actual_parameter other_actual_parameters RPAREN
                     ;
other_actual_parameters: COMMA actual_parameter other_actual_parameters
                       |
                       ;
actual_parameter: expression
                | string
                ;
structured_statement: compound_statement
                    | repetitive_statement
                    | if_statement
                    ;
compound_statement: BEGINNING statement_sequence END
                  ;
repetitive_statement: while_statement
                    | for_statement
                    ;
while_statement: WHILE conditional_expression DO statement
               ;
for_statement: FOR IDENTIFIER ASSIGNMENT expression TO expression DO statement
             | FOR IDENTIFIER ASSIGNMENT expression DOWNTO expression DO statement
             ;
if_statement: IF conditional_expression THEN statement
            | IF conditional_expression THEN statement ELSE statement
            ;
conditional_expression: simple_expression relational_operator simple_expression
                     | variable
                     ;
expression: simple_expression relational_operator simple_expression
          | simple_expression
          ;
simple_expression: term additional_terms
                 ;
additional_terms: addition_operator term additional_terms
                |
                ;
addition_operator: PLUS
                 | MINUS
                 | OR
                 ;
term: factor additional_factors
    | sign factor additional_factors
    ;
additional_factors: multiplication_operator factor additional_factors
                  | multiplication_operator sign factor additional_factors
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
      | LPAREN expression RPAREN
      | NOT factor
      | NOT sign factor
      ;
variable: IDENTIFIER
        | indexed_variable
        ;
indexed_variable: variable LSQUAREPAREN expression RSQUAREPAREN
                ;
number: integer_number
      | real_number
      ;
integer_number: digit_sequence
              ;
real_number: digit_sequence DOT digit_sequence
           | digit_sequence DOT scale_factor
           | digit_sequence DOT digit_sequence scale_factor
           | digit_sequence scale_factor
           ;
scale_factor: E digit_sequence
            | E sign digit_sequence
            ;
unsigned_digit_sequence: DIGIT
                       | DIGIT unsigned_digit_sequence
                       ;
digit_sequence: unsigned_digit_sequence
              ;
string: DQUOTE string_character additional_string_characters DQUOTE
      ;
char: DQUOTE string_character DQUOTE
      ;
additional_string_characters: string_character additional_string_characters
                            |
                            ;
string_character: ANY_CHARACTER_EXCEPT_QUOTE
                ;
constant: number
        | sign number
        ;
sign: PLUS
    | MINUS
    ;
%%

int main(int argc, char *argv[]) {
  // looks like it defaults to stdin incase a file is not provided or is missing
  char *filename = argv[1];
  FILE *file = fopen(filename, "r");
  yyin = file;
  yyparse();
  return 0;
}

void yyerror() {
  printf("syntax error\n");
}
