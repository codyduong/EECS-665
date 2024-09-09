/* definitions and declarations */
%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace {EECS}
%define parser_class_name {Parser}
%output "parser.cc"
%token-table

%code requires {
  namespace EECS {
    class Manager;
    class Scanner;
  }

#ifndef YY_NULLPTR
#define YY_NULLPTR 0
#endif
}

%parse-param { Scanner &scanner}
%parse-param { Manager &manager}

%code{
  #include <iostream>
  #include "string.h"
  #include <fstream>
  #include <cstdlib>

  #include "calc.hpp"

  #undef yylex
  #define yylex scanner.yylex
}

%union {
  int intVal;
  // bool transGood;
}

%token END 0
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token DIV
%token MULT
%token <intVal> INTLIT

%type <intVal> Equation Term Factor

%%
input:
  Equation END { 
    std::cout << "expression value: " << $1 << std::endl; 
    YYACCEPT;
  }
  ;

Equation : /* Add to this (and other) productions */
  Equation PLUS Term { $$ = $1 + $3; }
  | Equation MINUS Term { $$ = $1 - $3; }
  | Term { $$ = $1; }
  ;

Term:
  Term MULT Factor { $$ = $1 * $3; }
  | Term DIV Factor { 
    if ($3 == 0) {
      std::cerr << "Division by zero error\n";
      exit(EXIT_FAILURE);
    }
    $$ = $1 / $3; 
  }
  | Factor { $$ = $1; }
  ;

Factor:
  LPAR Equation RPAR { $$ = $2; }
  | INTLIT { $$ = $1; }
  ;

/* CF-productions and actions */
%%
/* Program stub -- code at end of the parser.cc*/
void
EECS::Parser::error(const std::string &err_message){
  std::cerr << "Parser Error: " << err_message << "\n";
}