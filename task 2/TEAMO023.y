%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
void yyerror();
%}

%token PROGRAM BEGIN END DOT SEMICOLON IDENTIFIER COMMA E

%%
%%

int main() {
  yyparse();
  return 0;
}

void yyerror() {
  printf("Syntax error\n");
}
