/* Definitions */
%{
#include "scanner.hpp"

#undef YY_DECL
#define YY_DECL int EECS::Scanner::yylex( EECS::Parser::semantic_type * const lval)

/* Rules section follows */

using TokenTag = EECS::Parser::token;

#define yyterminate(){ return TokenTag::END; }

%}

%option debug
%option nodefault
%option yyclass="EECS::Scanner"
%option noyywrap
%option c++

%%
%{
/* Code executed at beginning of yylex */
yylval = lval;
%}

[0-9]+ { yylval->intVal = atoi(yytext); 
	 return TokenTag::INTLIT; 
	}
[(] { return TokenTag::LPAR; }
[)] { return TokenTag::RPAR; }
[+] { return TokenTag::PLUS; }
[-] { return TokenTag::MINUS; }
[/] { return TokenTag::DIV; }
[*] { return TokenTag::MULT; }

. {
	std::string msg = "Bad char";
	msg += yytext;
	msg += "\n";
	error(0,0, msg);
  }
[\n] { }

%%
/* Stub program */
