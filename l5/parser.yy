/* definitions and declarations */
%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace {EECS}
%define parser_class_name {Parser}
%define parse.error verbose
%output "parser.cc"
%token-table 

%code requires{
	namespace EECS {
		class Manager;
		class Scanner;
	}
	#include <iostream>
	#include "string.h"
	#include "ast.hpp"

#ifndef YY_NULLPTR
#define YY_NULLPTR 0
#endif
}

%parse-param { Scanner &scanner}
%parse-param { Manager& manager}

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
	const char * strVal;
	EECS::ProgramNode * programNode;
	std::list<EECS::OpNode *> * opsList;
	EECS::OpNode * opNode;
	EECS::MathNode * mathNode;
}

%token END  0
%token <strVal> ID
%token <intVal> INTLIT
%token LPAR RPAR PLUS MINUS DIV MULT ORATE CALCULATE

%type <programNode> Program
%type <opsList> Ops
%type <opNode> Op
%type <mathNode> Expr Term Factor Base

%%
// swapped to op/expr/term/factor associativity rather than sum/sub/prod/quot/base
Program : Ops END { $$ = new EECS::ProgramNode($1); manager.root = $$; }
Ops     : Ops Op  { $1->push_back($2); $$ = $1; }
        | Op      { $$ = new std::list<EECS::OpNode *>(); $$->push_back($1); }
Op      : CALCULATE ID Expr { $$ = new EECS::CalculateNode(new EECS::IDNode($2), $3); }
        | ORATE ID          { $$ = new EECS::OrateNode(new EECS::IDNode($2)); }
Expr    : Expr PLUS Term    { $$ = new EECS::AddNode($1, $3); }
        | Expr MINUS Term   { $$ = new EECS::SubNode($1, $3); }
        | Term              { $$ = $1; }
Term    : Term MULT Factor  { $$ = new EECS::MultNode($1, $3); }
        | Term DIV Factor   { $$ = new EECS::DivNode($1, $3); }
        | Factor            { $$ = $1; }
Factor  : LPAR Expr RPAR    { $$ = $2; }
        | Base              { $$ = $1; }
Base    : INTLIT            { $$ = new EECS::NumNode($1); }
        | ID                { $$ = new EECS::IDNode($1); }
/* CF-productions and actions */
%%
/* Program stub -- code at end of the parser.cc*/
void 
EECS::Parser::error(const std::string &err_message){
	std::cerr << "Parser Error: " << err_message << "\n";
}
