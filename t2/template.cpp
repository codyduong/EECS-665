#include <iostream>
#include <stack>
#include <string>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <set>

// again we are using a template like last time, this time its in cpp
// nothing too crazy
// no header files... i read a paper once about header files and why they
// exist, and then proceeded to never use header files again LOL

//{{INIT_CODE}}

struct Symbol
{
    std::string type; // 'Terminal', 'NonTerminal', 'ActionTrigger', '$'
    std::string value;
};

struct ProductionRule
{
    std::string lhs;
    std::vector<Symbol> rhs;
};

bool isTerminal(const std::string &sym)
{
    static const std::set<std::string> terminals = //{{TERMINALS_SET}}
        ;
    return terminals.find(sym) != terminals.end();
}
bool isNonTerminal(const std::string &sym)
{
    static const std::set<std::string> nonTerminals = //{{NONTERMINALS_SET}}
        ;
    return nonTerminals.find(sym) != nonTerminals.end();
}
bool isActionTrigger(const std::string &sym)
{
    return !sym.empty() && sym[0] == '#';
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

std::map<std::pair<std::string, std::string>, ProductionRule> parsingTable = {
//{{PARSING_TABLE_ENTRIES}}
};

int main()
{
    // stdinput... im not writing a robust cli for this LOL
    std::string line;
    while (getline(std::cin, line))
    {
        std::istringstream iss(line);
        std::string token, rest;
        if (getline(iss, token))
        {
            size_t colonPos = token.find(':');
            std::string lexeme = "";
            if (colonPos != std::string::npos)
            {
                lexeme = token.substr(colonPos + 1);
                token = token.substr(0, colonPos);
            }
            tokens.emplace_back(token, lexeme);
        }
    }

    std::stack<std::string> parseStack;
    parseStack.push("$");
    parseStack.push("{{START_SYMBOL}}");
    getNextToken();
    while (!parseStack.empty())
    {
        std::string top = parseStack.top();
        if (isTerminal(top))
        {
            if (top == symbol)
            {
                parseStack.pop();
                getNextToken();
            }
            else
            {
                std::cerr << "Syntax error: expected " << top << " but found " << symbol << std::endl;
                return 1;
            }
        }
        else if (isNonTerminal(top))
        {
            auto key = make_pair(top, symbol);
            parseStack.pop();
            // std::cout << "[Debug] Rule: " << top << " with symbol " << symbol << std::endl;
            if (parsingTable.count(key))
            {
                auto pr = parsingTable[key];
                // LL(1) is trivial (once you just look at enough of other peoples code with the implementation LOL)
                for (auto it = pr.rhs.rbegin(); it != pr.rhs.rend(); ++it)
                {
                    if (it->type != "$")
                    {
                        parseStack.push(it->value);
                    }
                }
            }
            else
            {
                std::cerr << "Syntax error: no rule for " << top << " with symbol " << symbol << std::endl;
                return 1;
            }
        }
        else if (isActionTrigger(top))
        {
            int actionNum = stoi(top.substr(1));
            parseStack.pop();
            // std::cout << "[Debug] Running action: " << top << std::endl;
            //{{ACTION_CODE}}
        }
        else if (top == "$")
        {
            parseStack.pop();
        }
        else
        {
            std::cerr << "Unknown symbol: " << top << std::endl;
            return 1;
        }
    }
    return 0;
}
