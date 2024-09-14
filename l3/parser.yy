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
  #include <memory>

  namespace EECS {
    class Manager;
    class Scanner;
    class MathNode;
    class IntNode;
    class BinaryOpNode;
  }

#ifndef YY_NULLPTR
#define YY_NULLPTR 0
#endif
}

%parse-param { Scanner &scanner }
%parse-param { Manager &manager }

%code {
  #include <iostream>
  #include "string.h"
  #include <fstream>
  #include <cstdlib>
  #include "calc.hpp"
  #include "nodes.hpp"

  #undef yylex
  #define yylex scanner.yylex
}

%union {
  int intVal;
  MathNode * transNode;
}

%token END 0
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token DIV
%token MULT
%token <intVal> INTLIT

%type <transNode> Equation Term Factor

%%
input:
  Equation END {
    // std::cout << "expression value: " << $1->print() << std::endl;
    std::cout << "digraph G {\n";
    int nodeCount = 0;
    $1->dot(std::cout, nodeCount);
    std::cout << "}\n";
    YYACCEPT;
  }
  ;

Equation : Equation PLUS Term { $$ = new EECS::BinaryOpNode("+", $1, $3); }
         | Equation MINUS Term { $$ = new EECS::BinaryOpNode("-", $1, $3); }
         | Term { $$ = $1; }
         ;

Term : Term MULT Factor { $$ = new EECS::BinaryOpNode("*", $1, $3); }
     | Term DIV Factor { $$ = new EECS::BinaryOpNode("/", $1, $3); }
     | Factor { $$ = $1; }
     ;

Factor : LPAR Equation RPAR { $$ = $2; }
       | INTLIT { $$ = new EECS::IntNode($1); }
       ;

%%
/* Program stub -- code at end of the parser.cc */
void
EECS::Parser::error(const std::string &err_message) {
  std::cerr << "Parser Error: " << err_message << "\n";
}
