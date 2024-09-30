all: calc test

calc: parser.o lexer.o calc.o
	g++ -g -o calc calc.o lexer.o parser.o

test: calc
	./calc input.txt

%.o: %.cpp
	g++ -g -c $<

lexer.o: lexer.yy.cc
	g++ -g -c $< -o lexer.o

lexer.yy.cc: grammar.hh lexer.l
	flex --outfile lexer.yy.cc lexer.l

grammar.hh: parser.yy
	bison --defines=grammar.hh -d -v parser.yy

parser.cc: parser.yy
	bison --defines=grammar.hh -d -v parser.yy

.PHONY: clean test
clean: 
	rm -rf *.o *.cc *.hh calc parser.output
