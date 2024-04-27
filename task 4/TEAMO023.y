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
    INTEGER_TYPE,
    REAL_TYPE,
    BOOLEAN_TYPE,
    CHAR_TYPE,
    ARRAY_TYPE
  };

  enum MultiplicationOperator {
    MULTIPLY_SIGN,
    DIVIDE_SIGN,
    REMAINDER_SIGN,
    AND_SIGN
  };

  enum RelationalOperator {
    EQUAL_SIGN,
    NOT_EQUAL_SIGN,
    LESS_SIGN,
    LESS_OR_EQUAL_SIGN,
    GREATER_SIGN,
    GREATER_OR_EQUAL_SIGN
  };

  enum AdditionOperator {
    PLUS_OPERATOR,
    MINUS_OPERATOR,
    OR_SIGN
  };

  enum Sign {
    PLUS_SIGN,
    MINUS_SIGN
  };

  struct TypeInfo {
    enum Type type;
    // The remaining 3 attributes are only used in arrays
    enum Type valueType;
    int minIndex;
    int maxIndex;
  };

  struct IdentifierInfo {
    char* identifier;
    struct TypeInfo typeInfo;
    int valueHasBeenAssigned;
    int assignmentIsAllowed;
  };

  struct SymbolTable {
    struct IdentifierInfo variables[100];
    int size;
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

  struct VariableInfo {
    int symbolTableIndex;
    // The remaining 2 attributes are only used in indexed variables
    int isIndexed;
  };

  struct AdditionalFactor {
    enum MultiplicationOperator multiplicationOperator;
    int isNull;
    enum Type type;
  };

  struct AdditionalTerm {
    enum AdditionOperator additionOperator;
    int isNull;
    enum Type type;
  };

  struct ForControl {
    int controlIndex;
    int oldControlAssignmentStatus;
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
  struct VariableInfo variableInfo;
  enum Type valueType;
  enum MultiplicationOperator multiplicationOperator;
  enum RelationalOperator relationalOperator;
  enum AdditionOperator additionOperator;
  struct AdditionalFactor additionalFactor;
  struct AdditionalTerm additionalTerm;
  struct ForControl forControl;
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
%type <variableInfo> variable indexed_variable
%type <valueType> expression simple_expression term factor
%type <multiplicationOperator> multiplication_operator
%type <relationalOperator> relational_operator
%type <additionOperator> addition_operator
%type <additionalFactor> additional_factors
%type <additionalTerm> additional_terms
%type <forControl> for_control

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
                                                      symbolTable.variables[symbolTable.size].typeInfo = $3;
                                                      symbolTable.variables[symbolTable.size].valueHasBeenAssigned = 0;
                                                      symbolTable.variables[symbolTable.size].assignmentIsAllowed = 1;
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
assignment_statement: variable ASSIGNMENT expression  { if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type == ARRAY_TYPE && !$1.isIndexed) {
                                                          printf("Error: can't assign to array variable %s directly\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                          return 1;
                                                        }
                                                        
                                                        if (!$1.isIndexed) {
                                                          if (!(symbolTable.variables[$1.symbolTableIndex].typeInfo.type == $3 || (symbolTable.variables[$1.symbolTableIndex].typeInfo.type == REAL_TYPE && $3 == INTEGER_TYPE))) {
                                                            printf("Error: can't assign value of incompatible type to variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                            return 1;
                                                          }
                                                        } else {
                                                          if (!(symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType == $3 || (symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType == REAL_TYPE && $3 == INTEGER_TYPE))) {
                                                            printf("Error: can't assign value of incompatible type to index variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                            return 1;
                                                          }
                                                        }
                                                        if (symbolTable.variables[$1.symbolTableIndex].assignmentIsAllowed == 0) {
                                                          printf("Error: can't assign to loop control variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                        }
                                                        symbolTable.variables[$1.symbolTableIndex].valueHasBeenAssigned = 1; }
                    | variable ASSIGNMENT char  { free($3);
                                                  if (!$1.isIndexed) {
                                                    if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type == CHAR_TYPE) {
                                                      printf("Error: can't assign char literal to no char variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                      return 1;
                                                    }
                                                  } else {
                                                    if (symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType == CHAR_TYPE) {
                                                      printf("Error: can't assign char literal to no char variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                      return 1;
                                                    }
                                                  }
                                                  symbolTable.variables[$1.symbolTableIndex].valueHasBeenAssigned = 1; }
                    ;
procedure_statement: READ LPAREN variable RPAREN  { if (symbolTable.variables[$3.symbolTableIndex].assignmentIsAllowed == 0) {
                                                      printf("Error: can't assign to loop control variable %s\n", symbolTable.variables[$3.symbolTableIndex].identifier);
                                                    }
                                                    symbolTable.variables[$3.symbolTableIndex].valueHasBeenAssigned = 1; }
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
while_statement: WHILE expression DO statement  { if ($2 != BOOLEAN_TYPE) {
                                                    printf("Error: control expression of while loop must be boolean\n");
                                                    return 1;
                                                  } }
               ;
for_statement: for_control DO statement { symbolTable.variables[$1.controlIndex].valueHasBeenAssigned = $1.oldControlAssignmentStatus;
                                          symbolTable.variables[$1.controlIndex].assignmentIsAllowed = 1; }
             ;
for_control: FOR IDENTIFIER ASSIGNMENT expression TO expression { int variableIndex = -1;
                                                                  for (int i = 0; i < symbolTable.size; i++) {
                                                                    if (strcmp(symbolTable.variables[i].identifier, $2) == 0) {
                                                                      variableIndex = i;
                                                                      break;
                                                                    }
                                                                  }

                                                                  if (variableIndex == -1) {
                                                                    printf("Error: variable %s used before declaration\n", $2);
                                                                    free($2);
                                                                    return 1;
                                                                  }
                                                                  free($2);
                                                                  
                                                                  if (symbolTable.variables[variableIndex].typeInfo.type != INTEGER_TYPE) {
                                                                    printf("Error: non ordinal variable %s can't be used as for-loop control\n", symbolTable.variables[variableIndex].identifier);
                                                                    return 1;
                                                                  }
                                                                  
                                                                  if ($4 != INTEGER_TYPE || $6 != INTEGER_TYPE) {
                                                                    printf("Error: can't use non integral expression for for-loop control\n");
                                                                    return 1;
                                                                  }
                                                                  
                                                                  $$.oldControlAssignmentStatus = symbolTable.variables[variableIndex].valueHasBeenAssigned;
                                                                  $$.controlIndex = variableIndex;
                                                                  symbolTable.variables[variableIndex].valueHasBeenAssigned = 1;
                                                                  symbolTable.variables[variableIndex].assignmentIsAllowed = 0; }
           | FOR IDENTIFIER ASSIGNMENT expression DOWNTO expression { int variableIndex = -1;
                                                                      for (int i = 0; i < symbolTable.size; i++) {
                                                                        if (strcmp(symbolTable.variables[i].identifier, $2) == 0) {
                                                                          variableIndex = i;
                                                                          break;
                                                                        }
                                                                      }

                                                                      if (variableIndex == -1) {
                                                                        printf("Error: variable %s used before declaration\n", $2);
                                                                        free($2);
                                                                        return 1;
                                                                      }
                                                                      free($2);
                                                                      
                                                                      if (symbolTable.variables[variableIndex].typeInfo.type != INTEGER_TYPE) {
                                                                        printf("Error: non ordinal variable %s can't be used as for-loop control\n", symbolTable.variables[variableIndex].identifier);
                                                                        return 1;
                                                                      }
                                                                      
                                                                      if ($4 != INTEGER_TYPE || $6 != INTEGER_TYPE) {
                                                                        printf("Error: can't use non integral expression for for-loop control\n");
                                                                        return 1;
                                                                      }
                                                                      
                                                                      $$.oldControlAssignmentStatus = symbolTable.variables[variableIndex].valueHasBeenAssigned;
                                                                      $$.controlIndex = variableIndex;
                                                                      symbolTable.variables[variableIndex].valueHasBeenAssigned = 1;
                                                                      symbolTable.variables[variableIndex].assignmentIsAllowed = 0; }
           ;
if_statement: IF expression THEN statement  { if ($2 != BOOLEAN_TYPE) {
                                                printf("Error: if statement condition must be boolean\n");
                                                return 1;
                                              } }
            | IF expression THEN statement ELSE statement { if ($2 != BOOLEAN_TYPE) {
                                                            printf("Error: if statement condition must be boolean\n");
                                                            return 1;
                                                          } }
            ;
expression: simple_expression relational_operator simple_expression { if (!(($1 == INTEGER_TYPE || $1 == REAL_TYPE) && ($3 == INTEGER_TYPE || $3 == REAL_TYPE))) {
                                                                        printf("Error: can't apply relation operator on non-numeric value\n");
                                                                        return 1;
                                                                      }
                                                                      
                                                                      $$ = BOOLEAN_TYPE; }
          | simple_expression { $$ = $1; }
          ;
simple_expression: term additional_terms  { if (!$2.isNull) {
                                              switch ($2.additionOperator) {
                                                case OR_SIGN: 
                                                  if ($1 == BOOLEAN_TYPE && $2.type == BOOLEAN_TYPE) {
                                                    $$ = BOOLEAN_TYPE;
                                                  } else {
                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                    return 1;
                                                  }
                                                  break;

                                                case PLUS_OPERATOR:
                                                  if (!(($2.type == INTEGER_TYPE || $2.type == REAL_TYPE) && ($1 == INTEGER_TYPE || $1 == REAL_TYPE))) {
                                                    printf("Error: can't apply addition operator on non-numeric value\n");
                                                    return 1;
                                                  } else {
                                                    $$ = ($1 == REAL_TYPE || $2.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                  }
                                                  break;
                                                
                                                case MINUS_OPERATOR:
                                                  if (!(($2.type == INTEGER_TYPE || $2.type == REAL_TYPE) && ($1 == INTEGER_TYPE || $1 == REAL_TYPE))) {
                                                    printf("Error: can't apply subtraction operator on non-numeric value\n");
                                                    return 1;
                                                  } else {
                                                    $$ = ($1 == REAL_TYPE || $2.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                  }
                                                  break;
                                              }
                                            } else {
                                                $$ = $1;
                                            } }
                 ;
additional_terms: addition_operator term additional_terms { if (!$3.isNull) {
                                                              switch ($3.additionOperator) {
                                                                case OR_SIGN: 
                                                                  if ($2 == BOOLEAN_TYPE && $3.type == BOOLEAN_TYPE) {
                                                                    $$.type = BOOLEAN_TYPE;
                                                                  } else {
                                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                    return 1;
                                                                  }
                                                                  break;

                                                                case PLUS_OPERATOR:
                                                                  if (!(($3.type == INTEGER_TYPE || $3.type == REAL_TYPE) && ($2 == INTEGER_TYPE || $2 == REAL_TYPE))) {
                                                                    printf("Error: can't apply addition operator on non-numeric value\n");
                                                                    return 1;
                                                                  } else {
                                                                    $$.type = ($2 == REAL_TYPE || $3.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                  }
                                                                  break;
                                                                
                                                                case MINUS_OPERATOR:
                                                                  if (!(($3.type == INTEGER_TYPE || $3.type == REAL_TYPE) && ($2 == INTEGER_TYPE || $2 == REAL_TYPE))) {
                                                                    printf("Error: can't apply subtraction operator on non-numeric value\n");
                                                                    return 1;
                                                                  } else {
                                                                    $$.type = ($2 == REAL_TYPE || $3.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                  }
                                                                  break;
                                                              }
                                                            } else {
                                                                $$.type = $2;
                                                            }

                                                            $$.isNull = 0;
                                                            $$.additionOperator = $1; }
                | { $$.isNull = 1; }
                ;
addition_operator: PLUS { return PLUS_OPERATOR; }
                 | MINUS { return MINUS_OPERATOR; }
                 | OR { return OR_SIGN; }
                 ;
term: factor additional_factors { if (!$2.isNull) {
                                    switch ($2.multiplicationOperator) {
                                      case AND_SIGN: 
                                        if ($1 == BOOLEAN_TYPE && $2.type == BOOLEAN_TYPE) {
                                          $$ = BOOLEAN_TYPE;
                                        } else {
                                          printf("Error: can't apply boolean operator on non-boolean value\n");
                                          return 1;
                                        }
                                        break;

                                      case REMAINDER_SIGN: 
                                        if (!($1 == INTEGER_TYPE && $2.type == INTEGER_TYPE)){
                                          printf("Error: can't apply modulo operator on non-integer value\n");
                                          return 1;
                                        } else {
                                          $$ = INTEGER_TYPE;
                                        }
                                        break;

                                      case DIVIDE_SIGN:
                                        if (!(($2.type == INTEGER_TYPE || $2.type == REAL_TYPE) && ($1 == INTEGER_TYPE || $1 == REAL_TYPE))) {
                                          printf("Error: can't apply division operator on non-numeric value\n");
                                          return 1;
                                        } else {
                                          $$ = REAL_TYPE;
                                        }
                                        break;

                                      case MULTIPLY_SIGN:
                                        if (!(($2.type == INTEGER_TYPE || $2.type == REAL_TYPE) && ($1 == INTEGER_TYPE || $1 == REAL_TYPE))) {
                                          printf("Error: can't apply multiplication operator on non-numeric value\n");
                                          return 1;
                                        } else {
                                          $$ = ($1 == REAL_TYPE || $2.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                        }
                                        break;
                                    }
                                  } else {
                                      $$ = $1;
                                  } }
    | sign factor additional_factors  { if (!($2 == INTEGER_TYPE || $2 == REAL_TYPE)) {
                                          printf("Error: can't apply unary arithmetic operator on non-numeric value\n");
                                          return 1;
                                        }
                                        if (!$3.isNull) {
                                          switch ($3.multiplicationOperator) {
                                            case AND_SIGN: 
                                              printf("Error: can't apply boolean operator on non-boolean value\n");
                                              return 1;
                                              break;

                                            case REMAINDER_SIGN: 
                                              if (!($2 == INTEGER_TYPE && $3.type == INTEGER_TYPE)){
                                                printf("Error: can't apply modulo operator on non-integer value\n");
                                                return 1;
                                              } else {
                                                $$ = INTEGER_TYPE;
                                              }
                                              break;

                                            case DIVIDE_SIGN:
                                              if (!($3.type == INTEGER_TYPE || $3.type == REAL_TYPE)) {
                                                printf("Error: can't apply division operator on non-numeric value\n");
                                                return 1;
                                              } else {
                                                $$ = REAL_TYPE;
                                              }
                                              break;

                                            case MULTIPLY_SIGN:
                                              if (!($3.type == INTEGER_TYPE || $3.type == REAL_TYPE)) {
                                                printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                return 1;
                                              } else {
                                                $$ = ($2 == REAL_TYPE || $3.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                              }
                                              break;
                                          }
                                        } else {
                                          $$ = $2;
                                        } }
    ;
additional_factors: multiplication_operator factor additional_factors { if (!$3.isNull) {
                                                                          switch ($3.multiplicationOperator) {
                                                                            case AND_SIGN: 
                                                                              if ($2 == BOOLEAN_TYPE && $3.type == BOOLEAN_TYPE) {
                                                                                $$.type = BOOLEAN_TYPE;
                                                                              } else {
                                                                                printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                                return 1;
                                                                              }
                                                                              break;

                                                                            case REMAINDER_SIGN: 
                                                                              if (!($2 == INTEGER_TYPE && $3.type == INTEGER_TYPE)){
                                                                                printf("Error: can't apply modulo operator on non-integer value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = INTEGER_TYPE;
                                                                              }
                                                                              break;

                                                                            case DIVIDE_SIGN:
                                                                              if (!(($3.type == INTEGER_TYPE || $3.type == REAL_TYPE) && ($2 == INTEGER_TYPE || $2 == REAL_TYPE))) {
                                                                                printf("Error: can't apply division operator on non-numeric value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = REAL_TYPE;
                                                                              }
                                                                              break;

                                                                            case MULTIPLY_SIGN:
                                                                              if (!(($3.type == INTEGER_TYPE || $3.type == REAL_TYPE) && ($2 == INTEGER_TYPE || $2 == REAL_TYPE))) {
                                                                                printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = ($2 == REAL_TYPE || $3.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                              }
                                                                              break;
                                                                          }
                                                                        } else {
                                                                            $$.type = $2;
                                                                        }

                                                                        $$.isNull = 0;
                                                                        $$.multiplicationOperator = $1; }
                  | multiplication_operator sign factor additional_factors  { if (!($3 == INTEGER_TYPE || $3 == REAL_TYPE)) {
                                                                                printf("Error: can't apply unary arithmetic operator on non-numeric value\n");
                                                                                return 1;
                                                                              }

                                                                              if (!$4.isNull) {
                                                                                switch ($4.multiplicationOperator) {
                                                                                  case AND_SIGN: 
                                                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                                    return 1;
                                                                                    break;

                                                                                  case REMAINDER_SIGN: 
                                                                                    if (!($3 == INTEGER_TYPE && $4.type == INTEGER_TYPE)){
                                                                                      printf("Error: can't apply modulo operator on non-integer value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = INTEGER_TYPE;
                                                                                    }
                                                                                    break;

                                                                                  case DIVIDE_SIGN:
                                                                                    if (!($4.type == INTEGER_TYPE || $4.type == REAL_TYPE)) {
                                                                                      printf("Error: can't apply division operator on non-numeric value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = REAL_TYPE;
                                                                                    }
                                                                                    break;

                                                                                  case MULTIPLY_SIGN:
                                                                                    if (!($4.type == INTEGER_TYPE || $4.type == REAL_TYPE)) {
                                                                                      printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = ($3 == REAL_TYPE || $4.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                                    }
                                                                                    break;
                                                                                }
                                                                              } else {
                                                                                $$.type = $3;
                                                                              }

                                                                              $$.isNull = 0;
                                                                              $$.multiplicationOperator = $1; }
                  | { $$.isNull = 1; }
                  ;
multiplication_operator: MULTIPLY { $$ = MULTIPLY_SIGN; }
                       | DIVIDE { $$ = DIVIDE_SIGN; }
                       | REMAINDER { $$ = REMAINDER_SIGN; }
                       | AND { $$ = AND_SIGN; }
                       ;
relational_operator: EQUAL { $$ = EQUAL_SIGN; }
                   | NOT_EQUAL { $$ = NOT_EQUAL_SIGN; }
                   | LESS { $$ = LESS_SIGN; }
                   | LESS_OR_EQUAL { $$ = LESS_OR_EQUAL_SIGN; }
                   | GREATER { $$ = GREATER_SIGN; }
                   | GREATER_OR_EQUAL { $$ = GREATER_OR_EQUAL_SIGN; }
                   ;
factor: variable  { if(symbolTable.variables[$1.symbolTableIndex].typeInfo.type == ARRAY_TYPE && !$1.isIndexed) {
                      printf("Error: can't use array variable %s directly as a value\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                      return 1;
                    }
                    
                    if(!symbolTable.variables[$1.symbolTableIndex].valueHasBeenAssigned) {
                      printf("Error: value of variable %s used before assigning \n", symbolTable.variables[$1.symbolTableIndex].identifier);
                      return 1;
                    }
                    
                    if (!$1.isIndexed) {
                      $$ = symbolTable.variables[$1.symbolTableIndex].typeInfo.type;
                    } else {
                      $$ = symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType;
                    } }
      | number  { if ($1.type == INTEGER_TYPE) {
                    $$ = INTEGER_TYPE;
                  } else {
                    $$ = REAL_TYPE;
                  } }
      | LPAREN expression RPAREN { $$ = $2; }
      | NOT factor  { if ($2 != BOOLEAN_TYPE) {
                        printf("Error: can't use boolean operator NOT on non boolean value\n");
                        return 1;
                      }
                      
                      $$ = BOOLEAN_TYPE; }
      ;
variable: IDENTIFIER  { int variableIndex = -1;
                        for (int i = 0; i < symbolTable.size; i++) {
                          if (strcmp(symbolTable.variables[i].identifier, $1) == 0) {
                            variableIndex = i;
                            break;
                          }
                        }

                        if (variableIndex == -1) {
                          printf("Error: variable %s used before declaration\n", $1);
                          free($1);
                          return 1;
                        }

                        $$.isIndexed = 0;
                        $$.symbolTableIndex = variableIndex;
                        free($1); }
        | indexed_variable { $$ = $1; }
        ;
indexed_variable: variable LSQUAREPAREN expression RSQUAREPAREN { if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type != ARRAY_TYPE) {
                                                                    printf("Error: can't access indexed element of non-array variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                                    return 1;
                                                                  }
                                                                  
                                                                  if ($3 != INTEGER_TYPE) {
                                                                    printf("Error: can't access non-integer index of array variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                                    return 1;
                                                                  }

                                                                  $$.symbolTableIndex = $1.symbolTableIndex;
                                                                  $$.isIndexed = 1; }
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
                                                                        free($2);
                                                                      } }
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
                                                                                free($1);
                                                                              } }
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
