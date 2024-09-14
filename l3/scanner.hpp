#ifndef __EECS_SCANNER_HPP
#define __EECS_SCANNER_HPP

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "grammar.hh"

#include <istream>
#include <fstream>
#include <ostream>

using namespace std;

namespace EECS{
class Scanner : yyFlexLexer{
public:
	Scanner(std::istream * in) : yyFlexLexer(in){ 
		lineNum = 0;
		charNum = 0;
	}
	virtual ~Scanner() { }

	virtual int yylex(EECS::Parser::semantic_type * lval);

	void warn(int lineNum, int charNum, std::string msg){
		cerr << "lex problem at" << lineNum << "," << charNum
					<< "***WARNING*** " << msg << std::endl;
	}
	void error(int lineNum, int charNum, std::string msg){
		cerr << "lex problem at" << lineNum << "," << charNum
		<< "***ERROR*** " << msg << std::endl;
	}
private:
	EECS::Parser::semantic_type *yylval = nullptr;
	size_t lineNum;
	size_t charNum;
}; /* end class EECS::Scanner */

} /* end namespace */

#endif
