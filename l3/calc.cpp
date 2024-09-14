#include "calc.hpp"
#include <cassert>

void EECS::Manager::parse( const char * filepath){
	std::ifstream in_stream( filepath);

	this->scanner = new EECS::Scanner(&in_stream);
	this->parser = new EECS::Parser(*scanner, *this);

	std::cout << "Gettin' ready to parse\n";
	
	parser->parse();
	return;
}

int main(const int argc, const char *argv[] ){
	assert(argc > 1);

	EECS::Manager manager;
	manager.parse( argv[1]);
}
