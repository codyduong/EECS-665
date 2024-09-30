#include "calc.hpp"
#include "ast.hpp"
#include <cassert>

void EECS::Manager::parse( const char * filepath){
	std::ifstream in_stream( filepath);

	this->scanner = new EECS::Scanner(&in_stream);
	this->parser = new EECS::Parser(*scanner, *this);

	this->parser->parse();

	this->root->print();
	return;
}

int main(const int argc, const char *argv[] ){
	assert(argc > 1);

	EECS::Manager manager;
	manager.parse( "input.txt");
}
