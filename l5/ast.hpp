#ifndef EECS665_AST
#define EECS665_AST

#include <list>
#include <iostream>
#include <string>

namespace EECS {

class MathNode;
class IDNode;
class NumNode;

class OpNode {
public:
    virtual ~OpNode() = default;
    virtual void print() const = 0;
};

class ProgramNode {
public:
    std::list<OpNode *> ops;

    ProgramNode(std::list<OpNode *> *nodes) : ops(*nodes) {}
    
    void print() const {
        for (const auto& op : ops) {
            op->print();
        }
    }
};

class MathNode {
public:
    virtual ~MathNode() = default;
    virtual void print() const = 0;
};

class IDNode : public MathNode {
private:
    std::string id;
public:
    IDNode(const std::string &name) : id(name) {}
    void print() const override {
        std::cout << id;
    }
};

class NumNode : public MathNode {
private:
    int value;
public:
    NumNode(int val) : value(val) {}
    void print() const override {
        std::cout << value;
    }
};

class AddNode : public MathNode {
private:
    MathNode *left;
    MathNode *right;
public:
    AddNode(MathNode *l, MathNode *r) : left(l), right(r) {}
    void print() const override {
        left->print();
        std::cout << "+";
        right->print();
    }
};

class SubNode : public MathNode {
private:
    MathNode *left;
    MathNode *right;
public:
    SubNode(MathNode *l, MathNode *r) : left(l), right(r) {}
    void print() const override {
        left->print();
        std::cout << "-";
        right->print();
    }
};

class MultNode : public MathNode {
private:
    MathNode *left;
    MathNode *right;
public:
    MultNode(MathNode *l, MathNode *r) : left(l), right(r) {}
    void print() const override {
        left->print();
        std::cout << "*";
        right->print();
    }
};

class DivNode : public MathNode {
private:
    MathNode *left;
    MathNode *right;
public:
    DivNode(MathNode *l, MathNode *r) : left(l), right(r) {}
    void print() const override {
        left->print();
        std::cout << "/";
        right->print();
    }
};

class CalculateNode : public OpNode {
private:
    IDNode *id;
    MathNode *expression;
public:
    CalculateNode(IDNode *i, MathNode *exp) : id(i), expression(exp) {}
    void print() const override {
        std::cout << "calculate ";
        id->print();
        std::cout << " ";
        expression->print();
        std::cout << std::endl;
    }
};

class OrateNode : public OpNode {
private:
    IDNode *id;
public:
    OrateNode(IDNode *i) : id(i) {}
    void print() const override {
        std::cout << "orate ";
        id->print();
        std::cout << std::endl;
    }
};

} // namespace EECS

#endif