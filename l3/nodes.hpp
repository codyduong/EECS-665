#ifndef EECS_MATH_NODE_HH
#define EECS_MATH_NODE_HH

#include <string>
#include <iostream>
#include <list>
#include <memory>

namespace EECS
{
  class MathNode
  {
  public:
    MathNode() {}
    virtual ~MathNode() {}

    MathNode(std::string l1, MathNode *c1)
    {
      myChildren.push_back(std::make_pair(l1, c1));
    }

    MathNode(std::string l1, MathNode *c1,
             std::string l2, MathNode *c2)
    {
      myChildren.push_back(std::make_pair(l1, c1));
      myChildren.push_back(std::make_pair(l2, c2));
    }

    virtual int evaluate() const = 0;
    virtual std::string print() const = 0;
    virtual void dot(std::ostream &out, int &curentNodeIndex) const = 0;

  protected:
    std::list<std::pair<std::string, MathNode *>> myChildren;
  };

  class IntNode : public MathNode
  {
  public:
    IntNode(int valueIn) : value(valueIn), MathNode() {}
    std::string print() const override
    {
      return std::to_string(this->value);
    }
    int evaluate() const override
    {
      return value;
    }
    void dot(std::ostream &out, int &curentNodeIndex) const override
    {
      out << "  node" << curentNodeIndex << " [label=\"" << value << "\"];\n";
    }

  private:
    int value;
  };

  class BinaryOpNode : public MathNode
  {
  public:
    BinaryOpNode(std::string op, MathNode *left, MathNode *right)
        : operation(op), MathNode("arm1", left, "arm2", right) {}

    const std::string &getOperation() const { return operation; }
    std::string print() const override
    {
      std::string result = "(";
      result += myChildren.front().second->print() + " ";
      result += operation + " ";
      result += myChildren.back().second->print();
      result += ")";
      return result;
    }
    int evaluate() const override
    {
      int leftValue = myChildren.front().second->evaluate();
      int rightValue = myChildren.back().second->evaluate();

      if (operation == "+")
      {
        return leftValue + rightValue;
      }
      else if (operation == "-")
      {
        return leftValue - rightValue;
      }
      else if (operation == "*")
      {
        return leftValue * rightValue;
      }
      else if (operation == "/")
      {
        if (rightValue == 0)
        {
          throw std::runtime_error("Division by zero!");
        }
        return leftValue / rightValue;
      }
      else
      {
        throw std::runtime_error("Unknown operation: " + operation);
      }
    }

    void dot(std::ostream &out, int &nodeCount) const override
    {
      int currentId = nodeCount;
      out << "  node" << currentId << " [label=\"" << operation << "\"];\n";

      ++nodeCount;
      int leftId = nodeCount;
      myChildren.front().second->dot(out, nodeCount);
      out << "  node" << currentId << " -> node" << leftId << " [label=\"left\"];\n";

      ++nodeCount;
      int rightId = nodeCount;
      myChildren.back().second->dot(out, nodeCount);
      out << "  node" << currentId << " -> node" << rightId << " [label=\"right\"];\n";
    }

  private:
    std::string operation;
  };

}
#endif
