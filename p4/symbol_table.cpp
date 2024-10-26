#include "symbol_table.hpp"
namespace a_lang {

SemSymbol::SemSymbol(const std::string & name, TypeNode * type)
    : name(name), kind(VAR) {
    info.type = type;
}

// Constructor for function symbols
SemSymbol::SemSymbol(const std::string & name, FnDeclNode * funcDecl)
    : name(name), kind(FUNC) {
    info.fnDecl = funcDecl;
}

// Constructor for class symbols
SemSymbol::SemSymbol(const std::string & name, ClassDefnNode * classDefn)
    : name(name), kind(CLASS) {
    info.classDefn = classDefn;
}

std::string SemSymbol::getName() const {
    return name;
}

SemSymbol::Kind SemSymbol::getKind() const {
    return kind;
}

TypeNode * SemSymbol::getType() const {
    if (kind == VAR) {
        return info.type;
    }
    return nullptr;
}

FnDeclNode * SemSymbol::getFnDecl() const {
    if (kind == FUNC) {
        return info.fnDecl;
    }
    return nullptr;
}

ClassDefnNode * SemSymbol::getClassDefn() const {
    if (kind == CLASS) {
        return info.classDefn;
    }
    return nullptr;
}

ScopeTable::ScopeTable(){
	symbols = new HashMap<std::string, SemSymbol *>();
}

bool ScopeTable::insert(const std::string & name, SemSymbol * symbol) {
    if (symbols->find(name) != symbols->end()) {
        return false;
    } else {
        (*symbols)[name] = symbol;
        return true;
    }
}

SemSymbol * ScopeTable::find(const std::string & name) {
    auto it = symbols->find(name);
		if (it != symbols->end()) {
				return it->second;
		} else {
				return nullptr;
		}
}

SymbolTable::SymbolTable() {
    scopeTableChain = new std::list<ScopeTable *>();
}

void SymbolTable::enterScope() {
    scopeTableChain->push_front(new ScopeTable());
}

void SymbolTable::exitScope() {
    if (!scopeTableChain->empty()) {
        delete scopeTableChain->front();
        scopeTableChain->pop_front();
    }
}

bool SymbolTable::insert(const std::string & name, SemSymbol * symbol) {
    if (scopeTableChain->empty()) {
        return false; // No scope to insert into
    }
    return scopeTableChain->front()->insert(name, symbol);
}

SemSymbol * SymbolTable::find(const std::string & name) {
    for (auto scope : *scopeTableChain) {
        SemSymbol * symbol = scope->find(name);
        if (symbol != nullptr) {
            return symbol;
        }
    }
    return nullptr;
}

SemSymbol * SymbolTable::findInCurrentScope(const std::string & name) {
    if (scopeTableChain->empty()) {
        return nullptr;
    }
    return scopeTableChain->front()->find(name);
}
}
