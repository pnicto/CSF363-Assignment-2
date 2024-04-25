%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct SymbolTable symbolTable;
%}

%code requires {
  extern FILE *yyin;
  int yylex();
  void yyerror();

  enum Type {
    REAL_TYPE,
    INTEGER_TYPE,
    CHAR_TYPE,
    BOOLEAN_TYPE,
    ARRAY_TYPE
  };

  enum Sign {
    PLUS_SIGN,
    MINUS_SIGN
  };

  struct VariableInfo {
    char* identifier;
    enum Type type;
  };

  struct SymbolTable {
    struct VariableInfo variables[100];
    int size;
  };

  struct TypeInfo {
    enum Type type;
    // The remaining 3 attributes are only used in arrays
    enum Type valueType;
    int minIndex;
    int maxIndex;
  };

  struct IdentifierList {
    char* identifiers[100];
    int size;
  };

  struct NumericValue {
    enum Type type;
    int integerValue;
    float realValue;
  };

  struct Subrange {
    int minIndex;
    int maxIndex;
  };
}

%union {
  char* identifier;
  struct IdentifierList identifierList;
  struct TypeInfo varType;
  int integerValue;
  float realValue;
  struct NumericValue numericValue;
  enum Sign sign;
  struct Subrange subrange;
  char* stringValue;
}

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
%token PLUS MINUS E DQUOTE
%token <integerValue> DIGIT
%token <stringValue> ANY_CHARACTER_EXCEPT_QUOTE

// misc tokens (includes tokens that are shared by different groups)
%token DOT SEMICOLON COMMA LPAREN RPAREN LSQUAREPAREN RSQUAREPAREN ERROR
%token <identifier> IDENTIFIER

%type <identifierList> identifier_list
%type <varType> type array_type
%type <numericValue> constant number
%type <integerValue> unsigned_digit_sequence digit_sequence scale_factor integer_number
%type <realValue> real_number
%type <sign> sign
%type <subrange> subrange_type
%type <stringValue> string_character additional_string_characters char string

// Precedence rule for else shift-reduce conflict
%right THEN ELSE

%%
program: program_heading block DOT { printf("valid input\n"); return 0; }
       ;
program_heading: PROGRAM IDENTIFIER SEMICOLON { free($2); }
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
variable_declaration: identifier_list COLON type  { for (int i = 0; i < $1.size; i++) {
                                                      for (int j = 0; j < symbolTable.size; j++) {
                                                        if (strcmp($1.identifiers[i], symbolTable.variables[j].identifier) == 0) {
                                                          printf("Error: multiple declarations of variable %s\n", $1.identifiers[i]);

                                                          // Free the remaining identifiers not added to symbol table
                                                          for(int k = i; k < $1.size; k++) {
                                                            free($1.identifiers[k]);
                                                          }

                                                          return 1;
                                                        }
                                                      }

                                                      symbolTable.variables[symbolTable.size].identifier = $1.identifiers[i];
                                                      symbolTable.variables[symbolTable.size].type = $3.type;
                                                      symbolTable.size++;
                                                    } }
                    ;
identifier_list: IDENTIFIER COMMA identifier_list { for (int i = 0; i < $3.size; i++) {
                                                      if (strcmp($3.identifiers[i], $1) == 0) {
                                                        printf("Error: multiple declarations of variable %s\n", $3.identifiers[i]);

                                                        // Free the identifiers
                                                        for(int k = 0; k < $3.size; k++) {
                                                          free($3.identifiers[k]);
                                                        }

                                                        return 1;
                                                      }
                                                    }
                                                    
                                                    $$.size = 1 + $3.size;
                                                    $$.identifiers[0] = $1;
                                                    for (int i = 0; i < $3.size; i++) {
                                                      $$.identifiers[i+1] = $3.identifiers[i];
                                                    } }
               | IDENTIFIER { $$.size = 1; $$.identifiers[0] = $1; }
               ;
type: REAL { $$.type = REAL_TYPE; }
    | INTEGER { $$.type = INTEGER_TYPE; }
    | CHAR { $$.type = CHAR_TYPE; }
    | BOOLEAN { $$.type = BOOLEAN_TYPE; }
    | array_type { $$ = $1; }
    ;
array_type: ARRAY LSQUAREPAREN subrange_type RSQUAREPAREN OF type { if($6.type == ARRAY_TYPE) {
                                                                      printf("Error: array element type can't be another array\n");
                                                                      return 1;
                                                                    }
                                                                    
                                                                    $$.type = ARRAY_TYPE;
                                                                    $$.valueType = $6.type;
                                                                    $$.minIndex = $3.minIndex;
                                                                    $$.maxIndex = $3.maxIndex; }
          ;
subrange_type: constant DOTDOT constant { if (!($1.type == INTEGER_TYPE && $3.type == INTEGER_TYPE)) {
                                            printf("Error: array subrange indices must be integers\n");
                                            return 1;
                                          }

                                          if ($1.integerValue > $3.integerValue) {
                                            printf("Error: array's min index is larger than max index\n");
                                            return 1;
                                          }
                                          
                                          $$.minIndex = $1.integerValue;
                                          $$.maxIndex = $3.integerValue; }
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
                    | variable ASSIGNMENT char { free($3); }
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
                | string { free($1); }
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
while_statement: WHILE expression DO statement
               ;
for_statement: FOR IDENTIFIER ASSIGNMENT expression TO expression DO statement { free($2); }
             | FOR IDENTIFIER ASSIGNMENT expression DOWNTO expression DO statement { free($2); }
             ;
if_statement: IF expression THEN statement
            | IF expression THEN statement ELSE statement
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
variable: IDENTIFIER { free($1); }
        | indexed_variable
        ;
indexed_variable: variable LSQUAREPAREN expression RSQUAREPAREN
                ;
number: integer_number { $$.type = INTEGER_TYPE; $$.integerValue = $1; }
      | real_number { $$.type = REAL_TYPE; $$.realValue = $1; }
      ;
integer_number: digit_sequence { $$ = $1; }
              ;
real_number: digit_sequence DOT digit_sequence  { float decimalVal = $3;
                                                  while (decimalVal >= 1) {
                                                    decimalVal /= 10;
                                                  }
                                                  $$ = $1 + decimalVal; }
           | digit_sequence DOT scale_factor  { float realVal = $1;
                                                if ($3 > 0) {
                                                  for (int i = 0; i < $3; i++) {
                                                    realVal *= 10;
                                                  }
                                                } else {
                                                  for (int i = 0; i < $3; i++) {
                                                    realVal /= 10;
                                                  }
                                                }
                                                $$ = realVal; }
           | digit_sequence DOT digit_sequence scale_factor { float decimalVal = $3;
                                                              while (decimalVal >= 1) {
                                                                decimalVal /= 10;
                                                              }
                                                              float realVal = $1 + decimalVal;

                                                              if ($4 > 0) {
                                                                for (int i = 0; i < $4; i++) {
                                                                  realVal *= 10;
                                                                }
                                                              } else {
                                                                for (int i = 0; i < $4; i++) {
                                                                  realVal /= 10;
                                                                }
                                                              }
                                                              $$ = realVal; }
           | digit_sequence scale_factor  { float realVal = $1;
                                            if ($2 > 0) {
                                              for (int i = 0; i < $2; i++) {
                                                realVal *= 10;
                                              }
                                            } else {
                                              for (int i = 0; i < $2; i++) {
                                                realVal /= 10;
                                              }
                                            }
                                            $$ = realVal; }
           ;
scale_factor: E digit_sequence { $$ = $2; }
            | E sign digit_sequence { $$ = ($2 == PLUS_SIGN) ? $3 : -$3; }
            ;
unsigned_digit_sequence: DIGIT { $$ = $1; }
                       | unsigned_digit_sequence DIGIT { $$ = $1 * 10 + $2; }
                       ;
digit_sequence: unsigned_digit_sequence { $$ = $1; }
              ;
string: DQUOTE string_character additional_string_characters DQUOTE { if ($3 == NULL) $$ = $2;
                                                                      else {
                                                                        $$ = (char*) malloc(strlen($3) + 2);
                                                                        strcpy($$, $2);
                                                                        strcat($$, $3);
                                                                        free($3);
                                                                      } 
                                                                      free($2); }
      ;
char: DQUOTE string_character DQUOTE  { if(strlen($2) != 1) {
                                          printf("Error: character literal can only be a string of length 1\n");
                                          return 1;
                                        }
                                        
                                        $$ = $2; }
      ;
additional_string_characters: string_character additional_string_characters { if ($2 == NULL) $$ = $1;
                                                                              else {
                                                                                $$ = (char*) malloc(strlen($2) + 2);
                                                                                strcpy($$, $1);
                                                                                strcat($$, $2);
                                                                                free($2);
                                                                              } 
                                                                              free($1); }
                            | { $$ = NULL; }
                            ;
string_character: ANY_CHARACTER_EXCEPT_QUOTE { $$ = $1; }
                ;
constant: number { $$ = $1; }
        | sign number { $$ = $2;
                        if ($1 == MINUS_SIGN) {
                          if ($$.type == REAL_TYPE) {
                            $$.realValue *= -1;
                          } else {
                            $$.integerValue *= -1;
                          }
                        } }
        ;
sign: PLUS { $$ = PLUS_SIGN; }
    | MINUS { $$ = MINUS_SIGN; }
    ;
%%

int main(int argc, char *argv[]) {
  // initialize symbol table size as it doesn't seem to be possible outside main
  symbolTable.size = 0;

  // looks like it defaults to stdin incase a file is not provided or is missing
  char *filename = argv[1];
  FILE *file = fopen(filename, "r");
  yyin = file;
  yyparse();

  // TEMP DEBUG CODE
  /* printf("%d\n", symbolTable.size);
  for (int i = 0; i < symbolTable.size; i++) {
    printf("%s\n", symbolTable.variables[i].identifier);
    printf("%d\n", symbolTable.variables[i].type);
  } */

  // free the identifiers in symbol table
  for (int i = 0; i < symbolTable.size; i++) {
    free(symbolTable.variables[i].identifier);
  }

  return 0;
}

void yyerror() {
  printf("syntax error\n");
}
