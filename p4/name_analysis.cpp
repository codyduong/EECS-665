#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"

namespace a_lang{

//TODO here is a subset of the nodes needed to do nameAnalysis, 
// you should add the rest to allow for a complete treatment
// of any AST

bool ProgramNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    symTab->enterScope(); // Enter global scope

    for (DeclNode * decl : *myGlobals) {
        res = decl->nameAnalysis(symTab) && res;
    }

    symTab->exitScope(); // Exit global scope
    return res;
}

// VarDeclNode
bool VarDeclNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    std::string varName = myID->getName();

    // Check for invalid type (void)
    if (dynamic_cast<VoidTypeNode*>(myType)) {
        // Report error
        Report::fatal(myID->pos(), "Invalid type in declaration");
        res = false;
    }

    // Check for multiple declarations
    if (symTab->findInCurrentScope(varName)) {
        // Report error
        Report::fatal(myID->pos(), "Multiply declared identifier");
        res = false;
    }

    if (res) {
        // Create a new symbol and insert it
        SemSymbol * symbol = new SemSymbol(varName, myType);
        symTab->insert(varName, symbol);
        myID->attachSymbol(symbol);
    }

    // Name analysis for the initializer expression
    if (myInit) {
        res = myInit->nameAnalysis(symTab) && res;
    }

    return res;
}

// FormalDeclNode
bool FormalDeclNode::nameAnalysis(SymbolTable * symTab) {
    return VarDeclNode::nameAnalysis(symTab);
}

// FnDeclNode
bool FnDeclNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    std::string fnName = myID->getName();

    // Check for multiple declarations
    if (symTab->findInCurrentScope(fnName)) {
        // Report error
        Report::fatal(myID->pos(), "Multiply declared identifier");
        res = false;
    } else {
        // Create a new symbol and insert it
        SemSymbol * symbol = new SemSymbol(fnName, this);
        symTab->insert(fnName, symbol);
        myID->attachSymbol(symbol);
    }

    symTab->enterScope(); // Enter function scope

    // Name analysis for parameters
    for (FormalDeclNode * param : *myFormals) {
        res = param->nameAnalysis(symTab) && res;
    }

    // Name analysis for return type
    if (myRetType) {
        res = myRetType->nameAnalysis(symTab) && res;
    }

    // Name analysis for function body
    for (StmtNode * stmt : *myBody) {
        res = stmt->nameAnalysis(symTab) && res;
    }

    symTab->exitScope(); // Exit function scope
    return res;
}

// ClassDefnNode
bool ClassDefnNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    std::string className = myID->getName();

    // Check for multiple declarations
    if (symTab->findInCurrentScope(className)) {
        // Report error
        Report::fatal(myID->pos(), "Multiply declared identifier");
        res = false;
    } else {
        // Create a new symbol and insert it
        SemSymbol * symbol = new SemSymbol(className, this);
        symTab->insert(className, symbol);
        myID->attachSymbol(symbol);
    }

    symTab->enterScope(); // Enter class scope

    // Name analysis for class members
    for (DeclNode * member : *myMembers) {
        res = member->nameAnalysis(symTab) && res;
    }

    symTab->exitScope(); // Exit class scope
    return res;
}

// IDNode
bool IDNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    std::string name = getName();

    SemSymbol * symbol = symTab->find(name);
    if (!symbol) {
        // Report error
        Report::fatal(pos(), "Undeclared identifier");
        res = false;
    } else {
        attachSymbol(symbol);
    }

    return res;
}

// AssignStmtNode
bool AssignStmtNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myDst->nameAnalysis(symTab) && res;
    res = mySrc->nameAnalysis(symTab) && res;
    return res;
}

// MaybeStmtNode
bool MaybeStmtNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myDst->nameAnalysis(symTab) && res;
    res = mySrc1->nameAnalysis(symTab) && res;
    res = mySrc2->nameAnalysis(symTab) && res;
    return res;
}

// FromConsoleStmtNode
bool FromConsoleStmtNode::nameAnalysis(SymbolTable * symTab) {
    return myDst->nameAnalysis(symTab);
}

// ToConsoleStmtNode
bool ToConsoleStmtNode::nameAnalysis(SymbolTable * symTab) {
    return mySrc->nameAnalysis(symTab);
}

// PostDecStmtNode
bool PostDecStmtNode::nameAnalysis(SymbolTable * symTab) {
    return myLoc->nameAnalysis(symTab);
}

// PostIncStmtNode
bool PostIncStmtNode::nameAnalysis(SymbolTable * symTab) {
    return myLoc->nameAnalysis(symTab);
}

// IfStmtNode
bool IfStmtNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myCond->nameAnalysis(symTab) && res;

    symTab->enterScope(); // Enter if block scope
    for (StmtNode * stmt : *myBody) {
        res = stmt->nameAnalysis(symTab) && res;
    }
    symTab->exitScope(); // Exit if block scope

    return res;
}

// IfElseStmtNode
bool IfElseStmtNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myCond->nameAnalysis(symTab) && res;

    symTab->enterScope(); // Enter if-true block scope
    for (StmtNode * stmt : *myBodyTrue) {
        res = stmt->nameAnalysis(symTab) && res;
    }
    symTab->exitScope(); // Exit if-true block scope

    symTab->enterScope(); // Enter else block scope
    for (StmtNode * stmt : *myBodyFalse) {
        res = stmt->nameAnalysis(symTab) && res;
    }
    symTab->exitScope(); // Exit else block scope

    return res;
}

// WhileStmtNode
bool WhileStmtNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myCond->nameAnalysis(symTab) && res;

    symTab->enterScope(); // Enter while block scope
    for (StmtNode * stmt : *myBody) {
        res = stmt->nameAnalysis(symTab) && res;
    }
    symTab->exitScope(); // Exit while block scope

    return res;
}

// ReturnStmtNode
bool ReturnStmtNode::nameAnalysis(SymbolTable * symTab) {
    if (myExp) {
        return myExp->nameAnalysis(symTab);
    }
    return true;
}

// CallExpNode
bool CallExpNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;

    res = myCallee->nameAnalysis(symTab) && res;

    for (ExpNode * arg : *myArgs) {
        res = arg->nameAnalysis(symTab) && res;
    }

    return res;
}

// MemberFieldExpNode
bool MemberFieldExpNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;

    res = myBase->nameAnalysis(symTab) && res;
    res = myField->nameAnalysis(symTab) && res;

    return res;
}

// BinaryExpNode
bool BinaryExpNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;

    res = myExp1->nameAnalysis(symTab) && res;
    res = myExp2->nameAnalysis(symTab) && res;

    return res;
}

// UnaryExpNode
bool UnaryExpNode::nameAnalysis(SymbolTable * symTab) {
    return myExp->nameAnalysis(symTab);
}

// Type Nodes (no action needed for built-in types)
bool IntTypeNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool BoolTypeNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool VoidTypeNode::nameAnalysis(SymbolTable * symTab) { return true; }

// ClassTypeNode
bool ClassTypeNode::nameAnalysis(SymbolTable * symTab) {
    bool res = true;
    res = myID->nameAnalysis(symTab) && res;
    return res;
}

// ImmutableTypeNode
bool ImmutableTypeNode::nameAnalysis(SymbolTable * symTab) {
    return mySub->nameAnalysis(symTab);
}

// RefTypeNode
bool RefTypeNode::nameAnalysis(SymbolTable * symTab) {
    return mySub->nameAnalysis(symTab);
}

// Literal Nodes (nothing to do)
bool IntLitNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool StrLitNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool TrueNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool FalseNode::nameAnalysis(SymbolTable * symTab) { return true; }
bool EhNode::nameAnalysis(SymbolTable * symTab) { return true; }

// CallStmtNode
bool CallStmtNode::nameAnalysis(SymbolTable * symTab) {
    return myCallExp->nameAnalysis(symTab);
}
}
