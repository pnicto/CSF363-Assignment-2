%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

struct SymbolTable symbolTable;

#define DEL 1
#define EPS 5
#define LMAX 1000
#define CMAX 1000
char graph[LMAX][CMAX];
int graphNumber = 0;
%}

%code requires {
  extern FILE *yyin;
  int yylex();
  void yyerror();

  // graph related declarations
  typedef enum { CONSTANT, ID, OPERATOR } NodeType;
   enum Type {
    INTEGER_TYPE,
    REAL_TYPE,
    BOOLEAN_TYPE,
    CHAR_TYPE,
    ARRAY_TYPE,
    STRING_TYPE // this is used only for the AST write procedure
  };

  typedef struct {
    union {
      int i;
      float r;
      char c;
      char* s;
    };
    enum Type type;
  } ConstantNode;

  typedef struct {
    char* name;
  } IdentifierNode;

  typedef struct {
    int opr;
    int nOperands;
    struct Node* operands[1];
  } OperatorNode;

  typedef struct Node {
    NodeType type;
    union {
      ConstantNode constant;
      IdentifierNode identifier;
      OperatorNode opr;
    };
  } Node;


  enum MultiplicationOperator {
    MULTIPLY_SIGN = 0,
    DIVIDE_SIGN,
    REMAINDER_SIGN,
    AND_SIGN
  };

  enum RelationalOperator {
    EQUAL_SIGN = 4,
    NOT_EQUAL_SIGN,
    LESS_SIGN,
    LESS_OR_EQUAL_SIGN,
    GREATER_SIGN,
    GREATER_OR_EQUAL_SIGN
  };

  enum AdditionOperator {
    PLUS_OPERATOR = 10,
    MINUS_OPERATOR,
    OR_SIGN
  };

  enum Sign {
    PLUS_SIGN = 13,
    MINUS_SIGN
  };

  typedef enum {
    IF_ELSE = 15,
    FOR_TO,
    FOR_DOWNTO
  } ExtraAstTypes;

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
    int isIndexed;
    Node* ast;
  };

  struct AdditionalFactor {
    enum MultiplicationOperator multiplicationOperator;
    int isNull;
    enum Type type;
    Node* ast;
  };

  struct AdditionalTerm {
    enum AdditionOperator additionOperator;
    int isNull;
    enum Type type;
    Node *ast;
  };

  struct ForControl {
    int controlIndex;
    int oldControlAssignmentStatus;
    Node *ast;
  };

  struct Expression {
    enum Type valueType;
    Node *ast;
  };

  struct ParameterList {
    Node* parameters[100];
    int size;
  };

  void graphInit(void);
  void graphFinish();
  void graphBox(char *s, int *w, int *h);
  void graphDrawBox(char *s, int c, int l);
  void graphDrawArrow(int c1, int l1, int c2, int l2);
  void drawNode(Node *p, int c, int l, int *ce, int *cm);
  int createAST(Node *p);

  // this file declarations
  Node *opr(int oper, int nops, ...);
  Node *id(char* name);
  Node *con(void* value, enum Type type);
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
  struct Expression expressionInfo;
  enum MultiplicationOperator multiplicationOperator;
  enum RelationalOperator relationalOperator;
  enum AdditionOperator additionOperator;
  struct AdditionalFactor additionalFactor;
  struct AdditionalTerm additionalTerm;
  struct ForControl forControl;
  struct ParameterList parameterList;
  Node* node;
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
%type <expressionInfo> expression simple_expression term factor
%type <multiplicationOperator> multiplication_operator
%type <relationalOperator> relational_operator
%type <additionOperator> addition_operator
%type <additionalFactor> additional_factors
%type <additionalTerm> additional_terms
%type <forControl> for_control

%type <node> statement simple_statement assignment_statement block statement_part statement_sequence procedure_statement structured_statement if_statement actual_parameter other_actual_parameters actual_parameter_list compound_statement repetitive_statement while_statement for_statement

// Precedence rule for else shift-reduce conflict
%right THEN ELSE

%%
program: program_heading block DOT { printf("valid input\n"); createAST($2); return 0; }
       ;
program_heading: PROGRAM IDENTIFIER SEMICOLON { free($2); }
               ;
block: declaration_part statement_part { $$ = $2; }
     ;
declaration_part:
                | variable_declaration_part
                ;
statement_part: BEGINNING statement_sequence END { $$ = $2; }
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
statement_sequence: statement SEMICOLON statement_sequence { $$ = opr(SEMICOLON, 2, $1, $3); }
                  | statement { $$ = $1; }
                  | { $$ = NULL; }
                  ;
statement: simple_statement { $$ = $1; }
         | structured_statement
         ;
simple_statement: assignment_statement { $$ = $1; }
                | procedure_statement { $$ = $1; }
                ;
assignment_statement: variable ASSIGNMENT expression  { if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type == ARRAY_TYPE && !$1.isIndexed) {
                                                          printf("Error: can't assign to array variable %s directly\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                          return 1;
                                                        }

                                                        if (!$1.isIndexed) {
                                                          if (!(symbolTable.variables[$1.symbolTableIndex].typeInfo.type == $3.valueType || (symbolTable.variables[$1.symbolTableIndex].typeInfo.type == REAL_TYPE && $3.valueType == INTEGER_TYPE))) {
                                                            printf("Error: can't assign value of incompatible type to variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                            return 1;
                                                          }
                                                        } else {
                                                          if (!(symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType == $3.valueType || (symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType == REAL_TYPE && $3.valueType == INTEGER_TYPE))) {
                                                            printf("Error: can't assign value of incompatible type to index variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                            return 1;
                                                          }
                                                        }
                                                        if (symbolTable.variables[$1.symbolTableIndex].assignmentIsAllowed == 0) {
                                                          printf("Error: can't assign to loop control variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                          return 1;
                                                        }
                                                        symbolTable.variables[$1.symbolTableIndex].valueHasBeenAssigned = 1;

                                                        $$ = opr(ASSIGNMENT, 2, id(symbolTable.variables[$1.symbolTableIndex].identifier), $3.ast);
                                                      }
                    | variable ASSIGNMENT char  { $$ = opr(ASSIGNMENT, 2, id(symbolTable.variables[$1.symbolTableIndex].identifier), con((void*)(&$3), CHAR_TYPE));
                                                  free($3);
                                                  if (!$1.isIndexed) {
                                                    if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type != CHAR_TYPE) {
                                                      printf("Error: can't assign char literal to non char variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                      return 1;
                                                    }
                                                  } else {
                                                    if (symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType != CHAR_TYPE) {
                                                      printf("Error: can't assign char literal to non char variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                      return 1;
                                                    }
                                                  }
                                                  symbolTable.variables[$1.symbolTableIndex].valueHasBeenAssigned = 1; }
                    ;
procedure_statement: READ LPAREN variable RPAREN  { if (symbolTable.variables[$3.symbolTableIndex].assignmentIsAllowed == 0) {
                                                      printf("Error: can't assign to loop control variable %s\n", symbolTable.variables[$3.symbolTableIndex].identifier);
                                                      return 1;
                                                    }

                                                    if($3.isIndexed){
                                                      $$ = opr(READ, 1, $3.ast);
                                                    } else {
                                                      $$ = opr(READ, 1, id(symbolTable.variables[$3.symbolTableIndex].identifier));
                                                    }

                                                    symbolTable.variables[$3.symbolTableIndex].valueHasBeenAssigned = 1; }
                   | WRITE actual_parameter_list { $$ = opr(WRITE, 1, $2); }
                   ;
actual_parameter_list: LPAREN actual_parameter other_actual_parameters RPAREN { $$ = opr(COMMA, 2, $2, $3); }
                     ;
other_actual_parameters: COMMA actual_parameter other_actual_parameters { $$ = opr(COMMA, 2, $2, $3); }
                       | { $$ = NULL; }
                       ;
actual_parameter: expression { $$ = $1.ast; }
                | string { $$ = con((void*)($1), STRING_TYPE); }
                ;
structured_statement: compound_statement { $$ = $1; }
                    | repetitive_statement
                    | if_statement { $$ = $1; }
                    ;
compound_statement: BEGINNING statement_sequence END { $$ = $2; }
                  ;
repetitive_statement: while_statement { $$ = $1; }
                    | for_statement { $$ = $1; }
                    ;
while_statement: WHILE expression DO statement  { if ($2.valueType != BOOLEAN_TYPE) {
                                                    printf("Error: control expression of while loop must be boolean\n");
                                                    return 1;
                                                  }

                                                  $$ = opr(WHILE, 2, $2.ast, $4);
                                                }
               ;
for_statement: for_control DO statement { symbolTable.variables[$1.controlIndex].valueHasBeenAssigned = $1.oldControlAssignmentStatus;
                                          symbolTable.variables[$1.controlIndex].assignmentIsAllowed = 1;
                                          $$ = opr($1.ast->opr.opr, 3, $1.ast->opr.operands[0], $1.ast->opr.operands[1],$3);
                                        }
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

                                                                  if ($4.valueType != INTEGER_TYPE || $6.valueType != INTEGER_TYPE) {
                                                                    printf("Error: can't use non integral expression for for-loop control\n");
                                                                    return 1;
                                                                  }

                                                                  $$.oldControlAssignmentStatus = symbolTable.variables[variableIndex].valueHasBeenAssigned;
                                                                  $$.controlIndex = variableIndex;
                                                                  symbolTable.variables[variableIndex].valueHasBeenAssigned = 1;
                                                                  symbolTable.variables[variableIndex].assignmentIsAllowed = 0;

                                                                  Node* assignment = opr(ASSIGNMENT, 2, id(symbolTable.variables[variableIndex].identifier), $4.ast);
                                                                  $$.ast = opr(FOR_TO, 2, assignment, $6.ast);
                                                                }
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

                                                                      if ($4.valueType != INTEGER_TYPE || $6.valueType != INTEGER_TYPE) {
                                                                        printf("Error: can't use non integral expression for for-loop control\n");
                                                                        return 1;
                                                                      }

                                                                      $$.oldControlAssignmentStatus = symbolTable.variables[variableIndex].valueHasBeenAssigned;
                                                                      $$.controlIndex = variableIndex;
                                                                      symbolTable.variables[variableIndex].valueHasBeenAssigned = 1;
                                                                      symbolTable.variables[variableIndex].assignmentIsAllowed = 0;

                                                                      Node* assignment = opr(ASSIGNMENT, 2, id(symbolTable.variables[variableIndex].identifier), $4.ast);
                                                                      $$.ast = opr(FOR_DOWNTO, 2, assignment, $6.ast);
                                                                    }
           ;
if_statement: IF expression THEN statement  { if ($2.valueType != BOOLEAN_TYPE) {
                                                printf("Error: if statement condition must be boolean\n");
                                                return 1;
                                              }
                                              $$ = opr(IF, 2, $2.ast, $4);}
            | IF expression THEN statement ELSE statement { if ($2.valueType != BOOLEAN_TYPE) {
                                                            printf("Error: if statement condition must be boolean\n");
                                                            return 1;
                                                          }
                                                           $$ = opr(IF_ELSE, 3, $2.ast, $4, $6);
                                                          }
            ;
expression: simple_expression relational_operator simple_expression { if (!(($1.valueType == INTEGER_TYPE || $1.valueType == REAL_TYPE) && ($3.valueType == INTEGER_TYPE || $3.valueType == REAL_TYPE))) {
                                                                        printf("Error: can't apply relation operator on non-numeric value\n");
                                                                        return 1;
                                                                      }
                                                                      $$.ast = opr($2, 2, $1.ast, $3.ast);
                                                                      $$.valueType = BOOLEAN_TYPE; }
          | simple_expression { $$.valueType = $1.valueType;
                                $$.ast = $1.ast;
                              }
          ;
simple_expression: additional_terms term  { if (!$1.isNull) {
                                              switch ($1.additionOperator) {
                                                case OR_SIGN:
                                                  if ($2.valueType == BOOLEAN_TYPE && $1.type == BOOLEAN_TYPE) {
                                                    $$.valueType = BOOLEAN_TYPE;
                                                    $$.ast = opr(OR_SIGN, 2, $1.ast, $2.ast);
                                                  } else {
                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                    return 1;
                                                  }
                                                  break;

                                                case PLUS_OPERATOR:
                                                  if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                    printf("Error: can't apply addition operator on non-numeric value\n");
                                                    return 1;
                                                  } else {
                                                    $$.valueType = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                    $$.ast = opr(PLUS_OPERATOR, 2, $1.ast, $2.ast);
                                                  }
                                                  break;

                                                case MINUS_OPERATOR:
                                                  if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                    printf("Error: can't apply subtraction operator on non-numeric value\n");
                                                    return 1;
                                                  } else {
                                                    $$.valueType = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                    $$.ast = opr(MINUS_OPERATOR, 2, $1.ast, $2.ast);
                                                  }
                                                  break;
                                              }
                                            } else {
                                                $$.valueType = $2.valueType;
                                                $$.ast = $2.ast;
                                            } }
                 ;
additional_terms: additional_terms term addition_operator { if (!$1.isNull) {
                                                              switch ($1.additionOperator) {
                                                                case OR_SIGN:
                                                                  if ($2.valueType == BOOLEAN_TYPE && $1.type == BOOLEAN_TYPE) {
                                                                    $$.type = BOOLEAN_TYPE;
                                                                    $$.ast = opr(OR_SIGN, 2, $1.ast, $2.ast);
                                                                  } else {
                                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                    return 1;
                                                                  }
                                                                  break;

                                                                case PLUS_OPERATOR:
                                                                  if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                                    printf("Error: can't apply addition operator on non-numeric value\n");
                                                                    return 1;
                                                                  } else {
                                                                    $$.type = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                    $$.ast = opr(PLUS_OPERATOR, 2, $1.ast, $2.ast);
                                                                  }
                                                                  break;

                                                                case MINUS_OPERATOR:
                                                                  if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                                    printf("Error: can't apply subtraction operator on non-numeric value\n");
                                                                    return 1;
                                                                  } else {
                                                                    $$.type = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                    $$.ast = opr(MINUS_OPERATOR, 2, $1.ast, $2.ast);
                                                                  }
                                                                  break;
                                                              }
                                                            } else {
                                                                $$.type = $2.valueType;
                                                                $$.ast = $2.ast;
                                                            }

                                                            $$.isNull = 0;
                                                            $$.additionOperator = $3; }
                | { $$.isNull = 1; }
                ;
addition_operator: PLUS { $$ = PLUS_OPERATOR; }
                 | MINUS { $$ = MINUS_OPERATOR; }
                 | OR { $$ = OR_SIGN; }
                 ;
term: additional_factors factor { if (!$1.isNull) {
                                    switch ($1.multiplicationOperator) {
                                      case AND_SIGN:
                                        if ($2.valueType == BOOLEAN_TYPE && $1.type == BOOLEAN_TYPE) {
                                          $$.valueType = BOOLEAN_TYPE;
                                          $$.ast = opr(AND_SIGN, 2, $1.ast, $2.ast);
                                        } else {
                                          printf("Error: can't apply boolean operator on non-boolean value\n");
                                          return 1;
                                        }
                                        break;

                                      case REMAINDER_SIGN:
                                        if (!($2.valueType == INTEGER_TYPE && $1.type == INTEGER_TYPE)){
                                          printf("Error: can't apply modulo operator on non-integer value\n");
                                          return 1;
                                        } else {
                                          $$.valueType = INTEGER_TYPE;
                                          $$.ast = opr(REMAINDER_SIGN, 2, $1.ast, $2.ast);
                                        }
                                        break;

                                      case DIVIDE_SIGN:
                                        if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                          printf("Error: can't apply division operator on non-numeric value\n");
                                          return 1;
                                        } else {
                                          $$.valueType = REAL_TYPE;
                                          $$.ast = opr(DIVIDE_SIGN, 2, $1.ast, $2.ast);
                                        }
                                        break;

                                      case MULTIPLY_SIGN:
                                        if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                          printf("Error: can't apply multiplication operator on non-numeric value\n");
                                          return 1;
                                        } else {
                                          $$.valueType = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                          $$.ast = opr(MULTIPLY_SIGN, 2, $1.ast, $2.ast);
                                        }
                                        break;
                                    }
                                  } else {
                                      $$.valueType = $2.valueType;
                                      $$.ast = $2.ast;
                                  } }
    | additional_factors sign factor  { if (!($3.valueType == INTEGER_TYPE || $3.valueType == REAL_TYPE)) {
                                          printf("Error: can't apply unary arithmetic operator on non-numeric value\n");
                                          return 1;
                                        }
                                        if (!$1.isNull) {
                                          switch ($1.multiplicationOperator) {
                                            case AND_SIGN:
                                              printf("Error: can't apply boolean operator on non-boolean value\n");
                                              return 1;
                                              break;

                                            case REMAINDER_SIGN:
                                              if (!($3.valueType == INTEGER_TYPE && $1.type == INTEGER_TYPE)){
                                                printf("Error: can't apply modulo operator on non-integer value\n");
                                                return 1;
                                              } else {
                                                $$.valueType = INTEGER_TYPE;
                                                $$.ast = opr(REMAINDER_SIGN, 2, $1.ast, $3.ast);
                                              }
                                              break;

                                            case DIVIDE_SIGN:
                                              if (!($1.type == INTEGER_TYPE || $1.type == REAL_TYPE)) {
                                                printf("Error: can't apply division operator on non-numeric value\n");
                                                return 1;
                                              } else {
                                                $$.valueType = REAL_TYPE;
                                                $$.ast = opr(DIVIDE_SIGN, 2, $1.ast, $3.ast);
                                              }
                                              break;

                                            case MULTIPLY_SIGN:
                                              if (!($1.type == INTEGER_TYPE || $1.type == REAL_TYPE)) {
                                                printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                return 1;
                                              } else {
                                                $$.valueType = ($3.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                $$.ast = opr(MULTIPLY_SIGN, 2, $1.ast, $3.ast);
                                              }
                                              break;
                                          }
                                        } else {
                                          $$.valueType = $3.valueType;
                                          $$.ast = $3.ast;
                                        } }
    ;
additional_factors: additional_factors factor multiplication_operator { if (!$1.isNull) {
                                                                          switch ($1.multiplicationOperator) {
                                                                            case AND_SIGN:
                                                                              if ($2.valueType == BOOLEAN_TYPE && $1.type == BOOLEAN_TYPE) {
                                                                                $$.type = BOOLEAN_TYPE;
                                                                                $$.ast = opr(AND_SIGN, 2, $1.ast, $2.ast);
                                                                              } else {
                                                                                printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                                return 1;
                                                                              }
                                                                              break;

                                                                            case REMAINDER_SIGN:
                                                                              if (!($2.valueType == INTEGER_TYPE && $1.type == INTEGER_TYPE)){
                                                                                printf("Error: can't apply modulo operator on non-integer value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = INTEGER_TYPE;
                                                                                $$.ast = opr(REMAINDER_SIGN, 2, $1.ast, $2.ast);
                                                                              }
                                                                              break;

                                                                            case DIVIDE_SIGN:
                                                                              if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                                                printf("Error: can't apply division operator on non-numeric value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = REAL_TYPE;
                                                                                $$.ast = opr(DIVIDE_SIGN, 2, $1.ast, $2.ast);
                                                                              }
                                                                              break;

                                                                            case MULTIPLY_SIGN:
                                                                              if (!(($1.type == INTEGER_TYPE || $1.type == REAL_TYPE) && ($2.valueType == INTEGER_TYPE || $2.valueType == REAL_TYPE))) {
                                                                                printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                                                return 1;
                                                                              } else {
                                                                                $$.type = ($2.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                                $$.ast = opr(MULTIPLY_SIGN, 2, $1.ast, $2.ast);
                                                                              }
                                                                              break;
                                                                          }
                                                                        } else {
                                                                            $$.type = $2.valueType;
                                                                            $$.ast = $2.ast;
                                                                        }

                                                                        $$.isNull = 0;
                                                                        $$.multiplicationOperator = $3; }
                  | additional_factors sign factor multiplication_operator  { if (!($3.valueType == INTEGER_TYPE || $3.valueType == REAL_TYPE)) {
                                                                                printf("Error: can't apply unary arithmetic operator on non-numeric value\n");
                                                                                return 1;
                                                                              }

                                                                              if (!$1.isNull) {
                                                                                switch ($1.multiplicationOperator) {
                                                                                  case AND_SIGN:
                                                                                    printf("Error: can't apply boolean operator on non-boolean value\n");
                                                                                    return 1;
                                                                                    break;

                                                                                  case REMAINDER_SIGN:
                                                                                    if (!($3.valueType == INTEGER_TYPE && $1.type == INTEGER_TYPE)){
                                                                                      printf("Error: can't apply modulo operator on non-integer value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = INTEGER_TYPE;
                                                                                      $$.ast = opr(REMAINDER_SIGN, 2, $1.ast, $3.ast);
                                                                                    }
                                                                                    break;

                                                                                  case DIVIDE_SIGN:
                                                                                    if (!($1.type == INTEGER_TYPE || $1.type == REAL_TYPE)) {
                                                                                      printf("Error: can't apply division operator on non-numeric value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = REAL_TYPE;
                                                                                      $$.ast = opr(DIVIDE_SIGN, 2, $1.ast, $3.ast);
                                                                                    }
                                                                                    break;

                                                                                  case MULTIPLY_SIGN:
                                                                                    if (!($1.type == INTEGER_TYPE || $1.type == REAL_TYPE)) {
                                                                                      printf("Error: can't apply multiplication operator on non-numeric value\n");
                                                                                      return 1;
                                                                                    } else {
                                                                                      $$.type = ($3.valueType == REAL_TYPE || $1.type == REAL_TYPE) ? REAL_TYPE : INTEGER_TYPE;
                                                                                      $$.ast = opr(MULTIPLY_SIGN, 2, $1.ast, $3.ast);
                                                                                    }
                                                                                    break;
                                                                                }
                                                                              } else {
                                                                                $$.type = $3.valueType;
                                                                              }

                                                                              $$.isNull = 0;
                                                                              $$.multiplicationOperator = $4; }
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
                      $$.valueType = symbolTable.variables[$1.symbolTableIndex].typeInfo.type;
                      $$.ast = id(symbolTable.variables[$1.symbolTableIndex].identifier);
                    } else {
                      $$.valueType = symbolTable.variables[$1.symbolTableIndex].typeInfo.valueType;
                    } }
      | number  { if ($1.type == INTEGER_TYPE) {
                    $$.valueType = INTEGER_TYPE;
                    $$.ast = con((void*)(&($1.integerValue)), INTEGER_TYPE);
                  } else {
                    $$.valueType = REAL_TYPE;
                    $$.ast = con((void*)(&($1.realValue)), REAL_TYPE);
                  } }
      | LPAREN expression RPAREN { $$.valueType = $2.valueType;
                                  $$.ast = $2.ast;}
      | NOT factor  { if ($2.valueType != BOOLEAN_TYPE) {
                        printf("Error: can't use boolean operator NOT on non boolean value\n");
                        return 1;
                      }
                      $$.ast = opr(NOT, 1, $2.ast);
                      $$.valueType = BOOLEAN_TYPE;
                      }
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
        | indexed_variable { $$.ast = $1.ast; }
        ;
indexed_variable: variable LSQUAREPAREN expression RSQUAREPAREN { if (symbolTable.variables[$1.symbolTableIndex].typeInfo.type != ARRAY_TYPE) {
                                                                    printf("Error: can't access indexed element of non-array variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                                    return 1;
                                                                  }

                                                                  if ($3.valueType != INTEGER_TYPE) {
                                                                    printf("Error: can't access non-integer index of array variable %s\n", symbolTable.variables[$1.symbolTableIndex].identifier);
                                                                    return 1;
                                                                  }

                                                                  $$.symbolTableIndex = $1.symbolTableIndex;
                                                                  $$.isIndexed = 1;

                                                                  $$.ast = opr('a', 2, id(symbolTable.variables[$1.symbolTableIndex].identifier), $3.ast);
                                                                }
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
                                                  for (int i = 0; i < -$3; i++) {
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
                                                                for (int i = 0; i < -$4; i++) {
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
                                              for (int i = 0; i < -$2; i++) {
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


Node *opr(int oper, int nops, ...) {
  va_list ap;
  Node *node;
  if ((node = malloc(sizeof(Node) + (nops - 1) * sizeof(Node *))) == NULL)
    yyerror("out of memory");

  node->type = OPERATOR;
  node->opr.opr = oper;
  node->opr.nOperands = nops;

  va_start(ap, nops);
  for (int i = 0; i < nops; i++) {
    node->opr.operands[i] = va_arg(ap, Node *);
  }
  va_end(ap);

  return node;
}

Node *id(char *name) {
  Node *p;

  if ((p = malloc(sizeof(Node))) == NULL) yyerror("out of memory");

  p->type = ID;
  p->identifier.name = name;

  return p;
}

Node *con(void* value, enum Type type){
  Node *p;

  if ((p = malloc(sizeof(Node))) == NULL) yyerror("out of memory");

  p->type = CONSTANT;
  switch(type) {
    case INTEGER_TYPE:
      p->constant.i = *(int*)value;
      p->constant.type = INTEGER_TYPE;
      break;
    case REAL_TYPE:
      p->constant.r = *(float*)value;
      p->constant.type = REAL_TYPE;
      break;
    case CHAR_TYPE:
      p->constant.c = *(char*)value;
      p->constant.type = CHAR_TYPE;
      break;
    case STRING_TYPE:
      p->constant.s = (char*)value;
      p->constant.type = STRING_TYPE;
      break;
  }

  return p;
}

int createAST(Node *p) {
  int rte, rtm;

  graphInit();
  drawNode(p, 0, 0, &rte, &rtm);
  graphFinish();
  return 0;
}

void drawNode(Node *p, int c, int l, int *ce, int *cm) {
  int w, h;     /* node width and height */
  char *s;      /* node text */
  int cbar;     /* "real" start column of node (centred above subnodes) */
  int k;        /* child number */
  int che, chm; /* end column and mid of children */
  int cs;       /* start column of children */
  // TODO: Change this to a bigger limit probably
  char word[200]; /* extended node text */

  if (!p) return;
  strcpy(word, "???"); /* should never appear */
  s = word;

  switch (p->type) {
    case CONSTANT:
      switch(p->constant.type) {
        case INTEGER_TYPE:
          sprintf(word, "c(%d)", p->constant.i);
          break;
        case REAL_TYPE:
          sprintf(word, "c(%f)", p->constant.r);
          break;
        case CHAR_TYPE:
          sprintf(word, "c(%c)", p->constant.c);
          break;
        case STRING_TYPE:
          sprintf(word, "c(%s)", p->constant.s);
          break;
      }
      break;
    case ID:
      sprintf(word, "id(%s)", p->identifier.name);
      break;
    case OPERATOR:
      switch (p->opr.opr) {
        case IF:
          s = "if";
          break;
        case IF_ELSE:
          s = "if...else";
          break;
        case WHILE:
          s = "while";
          break;
        case FOR_TO:
          s = "for...to";
          break;
        case FOR_DOWNTO:
          s = "for...downto";
          break;
        case MULTIPLY_SIGN:
          s = "[*]";
          break;
        case DIVIDE_SIGN:
          s = "[/]";
          break;
        case REMAINDER_SIGN:
          s = "[%]";
          break;
        case AND_SIGN:
          s = "[and]";
          break;
        case EQUAL_SIGN:
          s = "[=]";
          break;
        case NOT_EQUAL_SIGN:
          s = "[<>]";
          break;
        case LESS_SIGN:
          s = "[<]";
          break;
        case LESS_OR_EQUAL_SIGN:
          s = "[<=]";
          break;
        case GREATER_SIGN:
          s = "[>]";
          break;
        case GREATER_OR_EQUAL_SIGN:
          s = "[>=]";
          break;
        case PLUS_OPERATOR:
          s = "[+]";
          break;
        case MINUS_OPERATOR:
          s = "[-]";
          break;
        case OR_SIGN:
          s = "[or]";
          break;
        case SEMICOLON:
          s = "[;]";
          break;
        case ASSIGNMENT:
          s = "[:=]";
          break;
        case COMMA:
          s = "[,]";
          break;
        case READ:
          s = "read";
          break;
        case WRITE:
          s = "write";
          break;
        case NOT:
          s = "not";
          break;
        case 'a':
          s = "array";
          break;
      }
      break;
  }

  graphBox(s, &w, &h);
  cbar = c;
  *ce = c + w;
  *cm = c + w / 2;

  /* node is leaf */
  if (p->type == CONSTANT || p->type == ID || p->opr.nOperands == 0) {
    graphDrawBox(s, cbar, l);
    return;
  }

  /* node has children */
  cs = c;
  for (k = 0; k < p->opr.nOperands; k++) {
    drawNode(p->opr.operands[k], cs, l + h + EPS, &che, &chm);
    cs = che;
  }

  /* total node width */
  if (w < che - c) {
    cbar += (che - c - w) / 2;
    *ce = che;
    *cm = (c + che) / 2;
  }

  /* draw node */
  graphDrawBox(s, cbar, l);

  /* draw arrows (not optimal: children are drawn a second time) */
  cs = c;
  for (k = 0; k < p->opr.nOperands; k++) {
    drawNode(p->opr.operands[k], cs, l + h + EPS, &che, &chm);
    graphDrawArrow(*cm, l + h, chm, l + h + EPS - 1);
    cs = che;
  }
}

void graphTest(int l, int c) {
  int ok;
  ok = 1;
  if (l < 0) ok = 0;
  if (l >= LMAX) ok = 0;
  if (c < 0) ok = 0;
  if (c >= CMAX) ok = 0;
  if (ok) return;
  printf("\n+++error: l=%d, c=%d not in drawing rectangle 0, 0 ... %d, %d", l,
         c, LMAX, CMAX);
  exit(1);
}

void graphInit(void) {
  int i, j;
  for (i = 0; i < LMAX; i++) {
    for (j = 0; j < CMAX; j++) {
      graph[i][j] = ' ';
    }
  }
}

void graphFinish() {
  int i, j;
  for (i = 0; i < LMAX; i++) {
    for (j = CMAX - 1; j > 0 && graph[i][j] == ' '; j--)
      ;
    graph[i][CMAX - 1] = 0;
    if (j < CMAX - 1) graph[i][j + 1] = 0;
    if (graph[i][j] == ' ') graph[i][j] = 0;
  }
  for (i = LMAX - 1; i > 0 && graph[i][0] == 0; i--)
    ;
  printf("\n\nGraph %d:\n", graphNumber++);
  for (j = 0; j <= i; j++) printf("\n%s", graph[j]);
  printf("\n");
}

void graphBox(char *s, int *w, int *h) {
  *w = strlen(s) + DEL;
  *h = 1;
}

void graphDrawBox(char *s, int c, int l) {
  int i;
  graphTest(l, c + strlen(s) - 1 + DEL);
  for (i = 0; i < strlen(s); i++) {
    graph[l][c + i + DEL] = s[i];
  }
}

void graphDrawArrow(int c1, int l1, int c2, int l2) {
  int m;
  graphTest(l1, c1);
  graphTest(l2, c2);
  m = (l1 + l2) / 2;
  while (l1 != m) {
    graph[l1][c1] = '|';
    if (l1 < l2)
      l1++;
    else
      l1--;
  }
  while (c1 != c2) {
    graph[l1][c1] = '-';
    if (c1 < c2)
      c1++;
    else
      c1--;
  }
  while (l1 != l2) {
    graph[l1][c1] = '|';
    if (l1 < l2)
      l1++;
    else
      l1--;
  }
  graph[l1][c1] = '|';
}
