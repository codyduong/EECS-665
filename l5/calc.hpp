#ifndef EECS_MANAGER_HPP
#define EECS_MANAGER_HPP

#include "scanner.hpp"
#include "grammar.hh"
#include "ast.hpp"

namespace EECS{

class Manager {
public:
	void parse(const char * filepath);
	EECS::ProgramNode * root = nullptr;
private:
	EECS::Scanner * scanner = nullptr;
	EECS::Parser * parser = nullptr;
}; /* end class manager */

} /* end namespace EECS */

#endif
