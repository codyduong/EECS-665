#include "ast.hpp"

namespace a_lang{

IRProgram * ProgramNode::to3AC(TypeAnalysis * ta){
	IRProgram * prog = new IRProgram(ta);
	for (auto global : *myGlobals){
		global->to3AC(prog);
	}
	return prog;
}

void FnDeclNode::to3AC(IRProgram * prog){
	SemSymbol * sym = ID()->getSymbol();
	prog->gatherGlobal(sym);
	Procedure * p = prog->makeProc(sym->getName());
	size_t k = 1;
	for (auto formal : *myFormals){
		formal->to3AC(p);
		SemSymbol * fsym = formal->ID()->getSymbol();
		SymOpd * o = p->getSymOpd(fsym);

		Quad * q = new GetArgQuad(k,o);
		p->addQuad(q);
		k = k + 1;
	}
	for (auto stmt : *myBody){
		stmt->to3AC(p);
	}
}

void FnDeclNode::to3AC(Procedure * proc){
	//This never needs to be implemented,
	// the function only exists because of
	// inheritance needs (A function declaration
	// never occurs within another function)
	throw new InternalError("FnDecl at a local scope");
}

void FormalDeclNode::to3AC(IRProgram * prog){
	//This never needs to be implemented,
	// the function only exists because of
	// inheritance needs (A formal never
	// occurs at global scope)
	throw new InternalError("Formal at a global scope");
}

void FormalDeclNode::to3AC(Procedure * proc){
	SemSymbol * sym = ID()->getSymbol();
	proc->gatherFormal(sym);
}

Opd * IntLitNode::flatten(Procedure * proc){
	const DataType * type = proc->getProg()->nodeType(this);
	return new LitOpd(std::to_string(myNum), 8);
}

Opd * StrLitNode::flatten(Procedure * proc){
	Opd * res = proc->getProg()->makeString(myStr);
	return res;
}

Opd * TrueNode::flatten(Procedure * proc){
	return new LitOpd("1", 8);
}

Opd * FalseNode::flatten(Procedure * proc){
	return new LitOpd("0", 8);
}

Opd * EhNode::flatten(Procedure * proc){
    AuxOpd * res = proc->makeTmp(8);
    Quad * q = new UnaryOpQuad(res, UnaryOp::NOT64, new LitOpd("0", 8));
    proc->addQuad(q);
    return res;
}

Opd * CallExpNode::flatten(Procedure * proc){
	size_t k = 1;
	for (auto formal : *myArgs){
		Opd * form = formal->flatten(proc);
		Quad * q = new SetArgQuad(k, form);
		proc->addQuad(q);
		k = k + 1;
	}
	SemSymbol * sym = myCallee->getSymbol();
	Quad * cq = new CallQuad(sym);
	proc->addQuad(cq); 
	AuxOpd * res = proc->makeTmp(8);
	Quad * q = new GetRetQuad(res);
	proc->addQuad(q);
	return res;
}

Opd * NegNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * expType = prog->nodeType(myExp);
	expType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * expOpd = myExp->flatten(proc);
	Quad * q = new UnaryOpQuad(res, UnaryOp::NEG64, expOpd);
	proc->addQuad(q);
	return res;
}

Opd * NotNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * expType = prog->nodeType(myExp);
	expType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * expOpd = myExp->flatten(proc);
	Quad * q = new UnaryOpQuad(res, UnaryOp::NOT64, expOpd);
	proc->addQuad(q);
	return res;
}

Opd * PlusNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::ADD64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * MinusNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::SUB64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * TimesNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::MULT64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * DivideNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::DIV64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * AndNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::AND64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * OrNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::OR64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * EqualsNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::EQ64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * NotEqualsNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::NEQ64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * LessNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::LT64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * GreaterNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::GT64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * LessEqNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::LTE64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

Opd * GreaterEqNode::flatten(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * lhsType = prog->nodeType(myExp1);
	const DataType * rhsType = prog->nodeType(myExp2);
	lhsType->isInt();
	rhsType->isInt();
	AuxOpd * res = proc->makeTmp(8);
	Opd * lhsOpd = myExp1->flatten(proc);
	Opd * rhsOpd = myExp2->flatten(proc);
	Quad * q = new BinOpQuad(res, BinOp::GTE64, lhsOpd, rhsOpd);
	proc->addQuad(q);
	return res;
}

void AssignStmtNode::to3AC(Procedure * proc){
	Opd * srcOpd = mySrc->flatten(proc);
	Opd * dstOpd = myDst->flatten(proc);
	Quad * q = new AssignQuad(dstOpd, srcOpd);
	proc->addQuad(q);
}

void MaybeStmtNode::to3AC(Procedure * proc){
    Opd * srcOpd1 = mySrc1->flatten(proc);
    Opd * srcOpd2 = mySrc2->flatten(proc);
    Opd * dstOpd = myDst->flatten(proc);
    AuxOpd * resultOpd = proc->makeTmp(8);
    Quad * conditionalQuad = new BinOpQuad(resultOpd, BinOp::NEQ64, srcOpd1, srcOpd2);
    proc->addQuad(conditionalQuad);
    Quad * assignQuad = new AssignQuad(dstOpd, resultOpd);
    proc->addQuad(assignQuad);
}

void PostIncStmtNode::to3AC(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * locType = prog->nodeType(myLoc);
	locType->isInt();
	Opd * locOpd = myLoc->flatten(proc);
	LitOpd * res = new LitOpd("1",8);
	Quad * q = new BinOpQuad(locOpd, BinOp::ADD64,locOpd,res);
	proc->addQuad(q);
}

void PostDecStmtNode::to3AC(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * locType = prog->nodeType(myLoc);
	locType->isInt();
	Opd * locOpd = myLoc->flatten(proc);
	LitOpd * res = new LitOpd("1",8);
	Quad * q = new BinOpQuad(res, BinOp::SUB64, locOpd, res);
	proc->addQuad(q);
}

void ToConsoleStmtNode::to3AC(Procedure * proc){
	Opd * srcOpd = mySrc->flatten(proc);
	proc->addQuad(new IntrinsicQuad(OUTPUT, srcOpd));
}

void FromConsoleStmtNode::to3AC(Procedure * proc){
	Opd * dstOpd = myDst->flatten(proc);
	proc->addQuad(new IntrinsicQuad(INPUT, dstOpd));
}

void IfStmtNode::to3AC(Procedure * proc){
	Opd * condOpd = myCond->flatten(proc);
	Label * lbl = proc->makeLabel();
	Quad * nop = new NopQuad();
	nop->addLabel(lbl);
	Quad * q = new IfzQuad(condOpd,lbl);
	proc->addQuad(q);
	for (auto stmt : *myBody){
		stmt->to3AC(proc);
	}
	proc->addQuad(nop);
}

void IfElseStmtNode::to3AC(Procedure * proc){
	Opd * condOpd = myCond->flatten(proc);
	Label * firstlbl = proc->makeLabel();
	Quad * firstnop = new NopQuad();
	firstnop->addLabel(firstlbl);
	Label * seclbl = proc->makeLabel();
	Quad * secnop = new NopQuad();
	secnop->addLabel(seclbl);
	Quad * fq = new IfzQuad(condOpd,firstlbl);
	Quad * sq = new IfzQuad(condOpd,seclbl);
	Quad * gotoq = new GotoQuad(seclbl);
	proc->addQuad(fq);
	for (auto stmt : *myBodyTrue){
		stmt->to3AC(proc);
	}
	proc->addQuad(gotoq);
	proc->addQuad(firstnop);
	for (auto stmt : *myBodyFalse){
		stmt->to3AC(proc);
	}
	proc->addQuad(secnop);
}

void WhileStmtNode::to3AC(Procedure * proc){
	Label * firstlbl = proc->makeLabel();
	Quad * firstnop = new NopQuad();
	firstnop->addLabel(firstlbl);
	Label * seclbl = proc->makeLabel();
	Quad * secnop = new NopQuad();
	secnop->addLabel(seclbl);
	proc->addQuad(firstnop);
	Opd * condOpd = myCond->flatten(proc);
	Quad * fq = new IfzQuad(condOpd,firstlbl);
	Quad * sq = new IfzQuad(condOpd,seclbl);
	Quad * gotoq = new GotoQuad(firstlbl);
	proc->addQuad(sq);
	for (auto stmt : *myBody){
		stmt->to3AC(proc);
	}
	proc->addQuad(gotoq);
	proc->addQuad(secnop);
}

void CallStmtNode::to3AC(Procedure * proc){
	Opd * callOpd = myCallExp->flatten(proc);
	if(callOpd != nullptr){
		Quad * pop = proc->popQuad();
	}
}

void ReturnStmtNode::to3AC(Procedure * proc){
	IRProgram * prog = proc->getProg();
	const DataType * handleType = prog->nodeType(myExp);
	Opd * retOpd = myExp->flatten(proc);
	Quad * q = new SetRetQuad(retOpd);
	proc->addQuad(q);
	Label * leavelbl = proc->getLeaveLabel();
	Quad * gotoLeave = new GotoQuad(leavelbl);
	proc->addQuad(gotoLeave);
}

void VarDeclNode::to3AC(Procedure * proc){
	SemSymbol * sym = ID()->getSymbol();
	assert(sym != nullptr);
	proc->gatherLocal(sym);
}

void VarDeclNode::to3AC(IRProgram * prog){
	SemSymbol * sym = ID()->getSymbol();
	assert(sym != nullptr);
	prog->gatherGlobal(sym);
}

//We only get to this node if we are in a stmt
// context (DeclNodes protect descent)
Opd * IDNode::flatten(Procedure * proc){
	SemSymbol * sym = this->getSymbol();
	Opd * res = proc->getSymOpd(sym);
	if(res){
		return res;
	}else{
		throw new InternalError("Null ID");
	}
}

}
