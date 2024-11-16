#include "ast.hpp"
#include "symbol_table.hpp"
#include "errors.hpp"
#include "types.hpp"
#include "name_analysis.hpp"
#include "type_analysis.hpp"
#include "assert.h"

namespace a_lang{

TypeAnalysis * TypeAnalysis::build(NameAnalysis * nameAnalysis){
	//To emphasize that type analysis depends on name analysis
	// being complete, a name analysis must be supplied for 
	// type analysis to be performed.
	TypeAnalysis * typeAnalysis = new TypeAnalysis();
	auto ast = nameAnalysis->ast;	
	typeAnalysis->ast = ast;

	ast->typeAnalysis(typeAnalysis);
	if (typeAnalysis->hasError){
		return nullptr;
	}

	return typeAnalysis;

}

void ProgramNode::typeAnalysis(TypeAnalysis * ta){

	//pass the TypeAnalysis down throughout
	// the entire tree, getting the types for
	// each element in turn and adding them
	// to the ta object's hashMap
	for (auto global : *myGlobals){
		global->typeAnalysis(ta);
	}

	//The type of the program node will never
	// be needed. We can just set it to VOID
	//(Alternatively, we could make our type 
	// be error if the DeclListNode is an error)
	ta->nodeType(this, BasicType::produce(VOID));
}

void FnDeclNode::typeAnalysis(TypeAnalysis * ta){
	for (auto stmt : *myBody){
		stmt->typeAnalysis(ta);
	}
}

void StmtNode::typeAnalysis(TypeAnalysis * ta){
}

void AssignStmtNode::typeAnalysis(TypeAnalysis * ta){
	//TODO: Note that this function is incomplete. 
	// and needs additional code

	//Do typeAnalysis on the subexpressions
	myDst->typeAnalysis(ta);
	mySrc->typeAnalysis(ta);

	const DataType * tgtType = ta->nodeType(myDst);
	const DataType * srcType = ta->nodeType(mySrc);


	// As error returns null if subType is NOT an error type
	// otherwise, it returns the subType itself. It 
	// sort of serves as a way to cast the subtype
	if (tgtType->asError() || srcType->asError()){
		ta->nodeType(this, ErrorType::produce());
	}


	//While incomplete, this gives you one case for 
	// assignment: if the types are exactly the same
	// it is usually ok to do the assignment. One
	// exception is that if both types are function
	// names, it should fail type analysis
	if (tgtType == srcType){
		ta->nodeType(this, tgtType);
		return;
	}
	
	//Some functions are already defined for you to
	// report type errors. Note that these functions
	// also tell the typeAnalysis object that the
	// analysis has failed, meaning that main.cpp
	// will print "Type check failed" at the end
	if(tgtType->asFn() && srcType->isInt()){
        ta->nodeType(this, ErrorType::produce());
        ta->errAssignOpd(this->pos());
        } else if(tgtType->isInt() && srcType->asFn()){
            ta->nodeType(this, ErrorType::produce());
            ta->errAssignOpd(this->pos());
    }
	
	

    if(tgtType->isBool() && srcType->isInt()){
        ta->nodeType(this, ErrorType::produce());
        ta->errAssignOpr(this->pos());
        } else if(tgtType->isInt() && srcType->isBool()){
            ta->nodeType(this, ErrorType::produce());
            ta->errAssignOpr(this->pos());
    }     

    // Here, we set the type of the assignment
	// to void to indicate no error was found.
	// This step is optional, since you'll never
	// use the type of a statement
	ta->nodeType(this, BasicType::produce(VOID));
}

void ReturnStmtNode::typeAnalysis(TypeAnalysis * ta){
	const FnType * fnType = ta->getCurrentFnType();
	const DataType * fnRet = fnType->getReturnType();
	if (fnRet == BasicType::VOID()){
		if (myExp != nullptr) {
			myExp->typeAnalysis(ta);
			ta->extraRetValue(
				myExp->pos()); 
			ta->nodeType(this, ErrorType::produce());
		} else {
			ta->nodeType(this, BasicType::VOID());
		}
		return;
	}
	if (myExp == nullptr){
			ta->badNoRet(pos());
			ta->nodeType(this, ErrorType::produce());
			return;
	}
	myExp->typeAnalysis(ta);
	const DataType * childType = ta->nodeType(myExp);

	if (childType->asError()){
		ta->nodeType(this, ErrorType::produce());
		return;
	}
	if (childType != fnRet){
		ta->badRetValue(myExp->pos());
		ta->nodeType(this, ErrorType::produce());
		return;
	}
	ta->nodeType(this, ErrorType::produce());
	return;
}

void IfStmtNode::typeAnalysis(TypeAnalysis * ta){
    myCond->typeAnalysis(ta);
    const DataType * condInType = ta->nodeType(myCond); 

}

void IfElseStmtNode::typeAnalysis(TypeAnalysis * ta){
    myCond->typeAnalysis(ta);
    const DataType * condInType = ta->nodeType(myCond); 

}

void WhileStmtNode::typeAnalysis(TypeAnalysis * ta){
    myCond->typeAnalysis(ta);
    const DataType * condInType = ta->nodeType(myCond); 

}

void PostDecStmtNode::typeAnalysis(TypeAnalysis * ta){
    myLoc->typeAnalysis(ta);
    const DataType * inLocType = ta->nodeType(myLoc); 

    if(inLocType->isInt()){
        ta->nodeType(this, BasicType::INT());
    } 
    if(inLocType->asError()){
        ta->errMathOpd(pos());
        ta->nodeType(this, ErrorType::produce());
    }     
 	if(inLocType->isInt() == false){
            ta->errMathOpd(myLoc->pos());
            ta->nodeType(this, ErrorType::produce());
            
        }
}

void PostIncStmtNode::typeAnalysis(TypeAnalysis * ta){
    myLoc->typeAnalysis(ta);
    const DataType * inLocType = ta->nodeType(myLoc); 

    if(inLocType->isInt()){
        ta->nodeType(this, BasicType::INT());
    } 
    if(inLocType->asError()){
        ta->errMathOpd(pos());
        ta->nodeType(this, ErrorType::produce());
    }     
 	if(inLocType->isInt() == false){
            ta->errMathOpd(myLoc->pos());
            ta->nodeType(this, ErrorType::produce());
            
        }
}

void FromConsoleStmtNode::typeAnalysis(TypeAnalysis * ta){
	myDst->typeAnalysis(ta);
	const DataType * childType = ta->nodeType(myDst);
	const BasicType * childAsVar = childType->asBasic();

	if (childType->asBasic()){
		return;
	} else if (childType->asPtr()){
		ta->badWritePtr(myDst->pos());
		return;
	} else if (childType->asFn()){
		ta->badReadFn(myDst->pos());
		ta->nodeType(this, ErrorType::produce());
		return;
	} else {
		throw new InternalError("for type unaccounted");
	}
	ta->nodeType(this, BasicType::VOID());
}

void ToConsoleStmtNode::typeAnalysis(TypeAnalysis * ta){
	mySrc->typeAnalysis(ta);
	const DataType * childType = ta->nodeType(mySrc);
	if (childType->asError()){
		ta->nodeType(this, ErrorType::produce());
		return;
	}
	if (childType->isVoid()){
		ta->badWriteVoid(mySrc->pos());
		ta->nodeType(this, ErrorType::produce());
		return;
	} else if (childType->asFn()){
		ta->badWriteFn(mySrc->pos());
		ta->nodeType(this, ErrorType::produce());
		return;
	} else if (childType->asBasic()){
		return;
	}
	if (const PtrType * asPtr = childType->asPtr()){
		const DataType * deref = PtrType::derefType(asPtr);
		const BasicType * base = deref->asBasic();
		assert(base != nullptr);	
		if (base->isChar()){
			ta->nodeType(this, BasicType::VOID());
		} else {
			ta->badWritePtr(mySrc->pos());
		}
		return;
	}
	ta->nodeType(this, BasicType::VOID());
}

void ExpNode::typeAnalysis(TypeAnalysis * ta){
}

void CallExpNode::typeAnalysis(TypeAnalysis * ta){

	std::list<const DataType *> * aList = new std::list<const DataType *>();
	for (auto actual : *myArgs){
		actual->typeAnalysis(ta);
		aList->push_back(ta->nodeType(actual));
	}

	SemSymbol * calleeSym = myCallee->getSymbol();
	assert(calleeSym != nullptr);
	const DataType * calleeType = calleeSym->getDataType();
	const FnType * fnType = calleeType->asFn();
	if (fnType == nullptr){
		ta->badCallee(myCallee->pos());
		ta->nodeType(this, ErrorType::produce());
		return;
	}

	const std::list<const DataType *>* fList = fnType->getFormalTypes();
	if (aList->size() != fList->size()){
		ta->badArgCount(pos());
	} else {
		auto actualTypesItr = aList->begin();
		auto formalTypesItr = fList->begin();
		auto actualsItr = myArgs->begin();
		while(actualTypesItr != aList->end()){
			const DataType * actualType = *actualTypesItr;
			const DataType * formalType = *formalTypesItr;
			const ExpNode * actual = *actualsItr;
			actualTypesItr++;
			formalTypesItr++;
			actualsItr++;

			//Matching to error is ignored
			if (actualType->asError()){ continue; }
			if (formalType->asError()){ continue; }

			//Ok match
			if (formalType == actualType){ continue; }

			//Bad match
			ta->badArgMatch(actual->pos());
			ta->nodeType(this, ErrorType::produce());
		}
	}
	ta->nodeType(this, fnType->getReturnType());
	return;
}

void PlusNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errMathOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce()); 
        }
        if(rhsType->isInt() == false){
            ta->errMathOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void MinusNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errMathOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce()); 
        }
        if(rhsType->isInt() == false){
            ta->errMathOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void TimesNode::typeAnalysis(TypeAnalysis * ta){ 
   myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    }   
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errMathOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
            
        }
        if(rhsType->isInt() == false){
            ta->errMathOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void DivideNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errMathOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
        }
        if(rhsType->isInt() == false){
            ta->errMathOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void NegNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp->typeAnalysis(ta);
    const DataType * expInType = ta->nodeType(myExp);
    if(expInType->isInt()){
        ta->nodeType(this, BasicType::INT());
    } 
    if(expInType->asError()){
        ta->errMathOpd(pos());
        ta->nodeType(this, ErrorType::produce());
    }     
 	if(expInType->isInt() == false){
            ta->errMathOpd(myExp->pos());
            ta->nodeType(this, ErrorType::produce()); 
        }
}


void LessNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errRelOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
        }
        if(rhsType->isInt() == false){
            ta->errRelOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void LessEqNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errRelOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
        }
        if(rhsType->isInt() == false){
            ta->errRelOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void GreaterNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errRelOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());   
        }
        if(rhsType->isInt() == false){
            ta->errRelOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    } 
}

void GreaterEqNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isInt() && rhsType->isInt()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isInt() == false || rhsType->isInt() == false){
        if(lhsType->isInt() == false){
            ta->errRelOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());   
        }
        if(rhsType->isInt() == false){
            ta->errRelOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    }     
}

void AndNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isBool() && rhsType->isBool()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isBool() == false || rhsType->isBool() == false){
        if(lhsType->isBool() == false){
            ta->errLogicOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
        }
        if(rhsType->isBool() == false){
            ta->errLogicOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    }     
}

void OrNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->isBool() && rhsType->isBool()){
        ta->nodeType(this, BasicType::INT());
        return;
    } 
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
    } 
    if(lhsType->isBool() == false || rhsType->isBool() == false){
        if(lhsType->isBool() == false){
            ta->errLogicOpd(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
        }
        if(rhsType->isBool() == false){
            ta->errLogicOpd(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());         
        }
    }     
}

void NotNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp->typeAnalysis(ta);
    const DataType * expInType = ta->nodeType(myExp);
    if(expInType->isBool()){
        ta->nodeType(this, BasicType::INT());
    } 
    if(expInType->asError()){
        ta->errLogicOpd(pos());
        ta->nodeType(this, ErrorType::produce());
    }     
 	if(expInType->isBool() == false){
            ta->errLogicOpd(myExp->pos());
            ta->nodeType(this, ErrorType::produce());
        }
}


void EqualsNode::typeAnalysis(TypeAnalysis * ta){ 
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
        return;
    } 
    if(lhsType->asArray() && rhsType->asArray()){
        if(lhsType->asArray()){
            ta->errArrayEq(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
         }
        if(rhsType->asArray()){
            ta->errArrayEq(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());
        }
    }
    if(lhsType->asBasic() && rhsType->asBasic()){
        if(lhsType->asBasic()){
            ta->errFileEq(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
         }
        if(rhsType->asBasic()){
            ta->errFileEq(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());
        }
    }              
}


void NotEqualsNode::typeAnalysis(TypeAnalysis * ta){
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType * lhsType = ta->nodeType(myExp1);
    const DataType * rhsType = ta->nodeType(myExp2);
    if(lhsType->asError() || rhsType->asError()){
        ta->nodeType(this, ErrorType::produce());
        return;
    } 
    if(lhsType->asArray() && rhsType->asArray()){
        if(lhsType->asArray()){
            ta->errArrayEq(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
         }
        if(rhsType->asArray()){
            ta->errArrayEq(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());
        }
    }
    if(lhsType->asBasic() && rhsType->asBasic()){
        if(lhsType->asBasic()){
            ta->errFileEq(myExp1->pos());
            ta->nodeType(this, ErrorType::produce());
         }
        if(rhsType->asBasic()){
            ta->errFileEq(myExp2->pos());
            ta->nodeType(this, ErrorType::produce());
        }
    }              
}

void DeclNode::typeAnalysis(TypeAnalysis * ta){
}

void VarDeclNode::typeAnalysis(TypeAnalysis * ta){
	// VarDecls always pass type analysis, since they 
	// are never used in an expression. You may choose
	// to type them void (like this), as discussed in class
	ta->nodeType(this, BasicType::produce(VOID));
}

void IDNode::typeAnalysis(TypeAnalysis * ta){
	// IDs never fail type analysis and always
	// yield the type of their symbol (which
	// depends on their definition)
	ta->nodeType(this, this->getSymbol()->getDataType());
}

void IntLitNode::typeAnalysis(TypeAnalysis * ta){
	// IntLits never fail their type analysis and always
	// yield the type INT
	ta->nodeType(this, BasicType::produce(INT));
}

// For IntTypeNode
void IntTypeNode::typeAnalysis(TypeAnalysis *ta) {
    ta->nodeType(this, BasicType::produce(BaseType::INT));
}

// For BoolTypeNode
void BoolTypeNode::typeAnalysis(TypeAnalysis *ta) {
    ta->nodeType(this, BasicType::produce(BaseType::BOOL));
}

// For VoidTypeNode
void VoidTypeNode::typeAnalysis(TypeAnalysis *ta) {
    ta->nodeType(this, BasicType::produce(BaseType::VOID));
}

// For RefTypeNode
void RefTypeNode::typeAnalysis(TypeAnalysis * ta) {
    mySub->typeAnalysis(ta);
    ta->nodeType(this, RefType::produce(ta->nodeType(mySub)));
}

// For BinaryExpNode
void BinaryExpNode::typeAnalysis(TypeAnalysis *ta) {
    myExp1->typeAnalysis(ta);
    myExp2->typeAnalysis(ta);
    const DataType *leftType = ta->nodeType(myExp1);
    const DataType *rightType = ta->nodeType(myExp2);
    if (leftType == rightType && leftType->isInt()) {
        ta->nodeType(this, BasicType::produce(BaseType::INT));
    } else {
        ta->nodeType(this, ErrorType::produce());
    }
}

// For UnaryExpNode
void UnaryExpNode::typeAnalysis(TypeAnalysis *ta) {
    myExp->typeAnalysis(ta);
    const DataType *expType = ta->nodeType(myExp);
    if (expType->isInt()) {
        ta->nodeType(this, BasicType::produce(BaseType::INT));
    } else {
        ta->nodeType(this, ErrorType::produce());
    }
}

void ClassTypeNode::typeAnalysis(TypeAnalysis *ta) {
    // A ClassTypeNode should be analyzed based on its identifier's type.
    const DataType *type = myID->getSymbol()->getDataType();
    if (type->isClass()) {
        ta->nodeType(this, type);
    } else {
        ta->nodeType(this, ErrorType::produce());
    }
}

void MaybeStmtNode::typeAnalysis(TypeAnalysis * ta) {
    // Analyze the destination (myDst)
    myDst->typeAnalysis(ta);
    const DataType * dstType = ta->nodeType(myDst);

    // Analyze the first source (mySrc1)
    mySrc1->typeAnalysis(ta);
    const DataType * src1Type = ta->nodeType(mySrc1);

    // Analyze the second source (mySrc2)
    mySrc2->typeAnalysis(ta);
    const DataType * src2Type = ta->nodeType(mySrc2);

    // If any operand is an error type, propagate the error
    if (dstType->asError() || src1Type->asError() || src2Type->asError()) {
        ta->nodeType(this, ErrorType::produce());
        return;
    }

    // Ensure the types are compatible
    if (dstType != src1Type || dstType != src2Type) {
		ta->reportError(pos(), "Type mismatch error message");
        ta->nodeType(this, ErrorType::produce());
        return;
    }

    // If everything is valid, set the node's type
    ta->nodeType(this, dstType);
}

void ImmutableTypeNode::typeAnalysis(TypeAnalysis * ta) {
    mySub->typeAnalysis(ta);
    ta->nodeType(this, ImmutableType::produce(ta->nodeType(mySub)));
}

void MemberFieldExpNode::typeAnalysis(TypeAnalysis * ta) {
    myBase->typeAnalysis(ta);
    const DataType * baseType = ta->nodeType(myBase);
    if (baseType->asError()) {
        ta->nodeType(this, ErrorType::produce());
        return;
    }
    const ClassType * classType = baseType->asClass();
    if (classType == nullptr) {
        ta->reportError(pos(), "Member access applied to non-class type");
        ta->nodeType(this, ErrorType::produce());
        return;
    }
    SemSymbol * fieldSym = classType->fieldLookup(myField->getName());
    if (fieldSym == nullptr) {
        ta->reportError(pos(), "Field '" + myField->getName() + "' not found");
        ta->nodeType(this, ErrorType::produce());
        return;
    }

    ta->nodeType(this, fieldSym->getDataType());
}


}
