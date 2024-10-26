#ifndef A_LANG_SYMBOL_TABLE_HPP
#define A_LANG_SYMBOL_TABLE_HPP
#include <string>
#include <unordered_map>
#include <list>

//Use an alias template so that we can use
// "HashMap" and it means "std::unordered_map"
template <typename K, typename V>
using HashMap = std::unordered_map<K, V>;

using namespace std;

namespace a_lang {

//A semantic symbol, which represents a single
// variable, function, etc. Semantic symbols 
// exist for the lifetime of a scope in the 
// symbol table. 
class TypeNode;
class FnDeclNode;
class ClassDefnNode;

class SemSymbol {
public:
    enum Kind { VAR, FUNC, CLASS };

    // Constructor for variable symbols
    SemSymbol(const std::string & name, TypeNode * type);

    // Constructor for function symbols
    SemSymbol(const std::string & name, FnDeclNode * funcDecl);

    // Constructor for class symbols
    SemSymbol(const std::string & name, ClassDefnNode * classDefn);

    // Accessor methods
    std::string getName() const;
    Kind getKind() const;

    // Get the associated type or declaration node
    TypeNode * getType() const;
    FnDeclNode * getFnDecl() const;
    ClassDefnNode * getClassDefn() const;

private:
    std::string name;
    Kind kind;

    // Depending on the kind, one of these will be used
    union {
        TypeNode * type;            // For variables
        FnDeclNode * fnDecl;        // For functions
        ClassDefnNode * classDefn;  // For classes
    } info;
};

//A single scope. The symbol table is broken down into a 
// chain of scope tables, and each scope table holds 
// semantic symbols for a single scope. For example,
// the globals scope will be represented by a ScopeTable,
// and the contents of each function can be represented by
// a ScopeTable.
class ScopeTable {
	public:
		ScopeTable();
    SemSymbol *find(const std::string &name);
    bool insert(const std::string &name, SemSymbol *symbol);

  private:
    HashMap<std::string, SemSymbol *> * symbols;
};

class SymbolTable{
	public:
		SymbolTable();
    void enterScope();
    void exitScope();
    bool insert(const std::string &name, SemSymbol *symbol);
    SemSymbol *find(const std::string &name);
    SemSymbol *findInCurrentScope(const std::string &name);

  private:
    std::list<ScopeTable *> * scopeTableChain;
};

	
}

#endif
