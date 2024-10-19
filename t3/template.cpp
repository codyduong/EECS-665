// template.cpp
#include <iostream>
#include <stack>
#include <string>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <set>

//{{INIT_CODE}}

struct Symbol
{
  std::string type; // 'Terminal', 'NonTerminal', 'ActionTrigger', 'Epsilon'
  std::string value;
  int actionNum; // For ActionTrigger
};

struct ProductionRule
{
  std::string lhs;
  std::vector<Symbol> rhs;
};

enum ActionType
{
  Shift,
  Reduce,
  Accept
};

struct Action
{
  ActionType type;
  int state;         // For shift actions
  ProductionRule pr; // For reduce actions
};

// Function to execute action code
void executeAction(int actionNum)
{
  //{{ACTION_CODE}}
}

bool isTerminal(const std::string &sym)
{
  static const std::set<std::string> terminals = //{{TERMINALS_SET}}
      ;
  return terminals.find(sym) != terminals.end() || sym == "$";
}
bool isNonTerminal(const std::string &sym)
{
  static const std::set<std::string> nonTerminals = //{{NONTERMINALS_SET}}
      ;
  return nonTerminals.find(sym) != nonTerminals.end();
}

std::string look; // lexeme
std::string symbol;

std::vector<std::pair<std::string, std::string>> tokens;
size_t tokenIndex = 0;
std::string getNextToken()
{
  if (tokenIndex < tokens.size())
  {
    symbol = tokens[tokenIndex].first;
    look = tokens[tokenIndex].second;
    tokenIndex++;
    return look;
  }
  else
  {
    symbol = "$";
    look = "";
    return "$";
  }
}

std::map<std::pair<int, std::string>, Action> actionTable = {
    //{{ACTION_TABLE}}
};

std::map<std::pair<int, std::string>, int> gotoTable = {
    //{{GOTO_TABLE}}
};

int main()
{
  // Read tokens from stdin
  std::string line;
  while (getline(std::cin, line))
  {
    std::istringstream iss(line);
    std::string token;
    iss >> token;
    size_t colonPos = token.find(':');
    std::string lexeme = "";
    if (colonPos != std::string::npos)
    {
      lexeme = token.substr(colonPos + 1);
      token = token.substr(0, colonPos);
    }
    tokens.emplace_back(token, lexeme);
  }

  std::stack<int> parseStack;
  parseStack.push(0); // Initial state
  getNextToken();
  while (true)
  {
    int state = parseStack.top();
    auto key = std::make_pair(state, symbol);
    if (actionTable.count(key))
    {
      Action action = actionTable[key];
      if (action.type == Shift)
      {
        parseStack.push(action.state);
        getNextToken();
      }
      else if (action.type == Reduce)
      {
        const ProductionRule &pr = action.pr;
        int popCount = 0;
        for (const auto &sym : pr.rhs)
        {
          if (sym.type != "Epsilon")
            popCount++;
        }
        for (int i = 0; i < popCount; ++i)
        {
          parseStack.pop();
        }
        int state_prime = parseStack.top();
        auto gotoKey = std::make_pair(state_prime, pr.lhs);
        if (gotoTable.count(gotoKey))
        {
          parseStack.push(gotoTable[gotoKey]);
        }
        else
        {
          std::cerr << "Goto error: no entry for state " << state_prime << " and symbol " << pr.lhs << std::endl;
          std::cout << "rejected" << std::endl;
          return 1;
        }
        // Execute any action triggers in the RHS
        for (const auto &sym : pr.rhs)
        {
          if (sym.type == "ActionTrigger")
          {
            int actionNum = sym.actionNum;
            executeAction(actionNum);
          }
        }
      }
      else if (action.type == Accept)
      {
        std::cout << "accepted" << std::endl;
        return 0;
      }
    }
    else
    {
      std::cout << "rejected" << std::endl;
      return 1;
    }
  }
  return 0;
}
