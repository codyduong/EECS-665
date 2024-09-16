all: clean build run test
.PHONY: all clean build run test

build:
	cabal build exe:dragonlex
	cp `cabal exec -- which dragonlex` ./dragonlex

run:
	./dragonlex A.spec
	cabal build exe:lexer
	cp `cabal exec -- which lexer` ./lexer

test:
	# ./lexer test1.txt
	./lexer test.txt > test.tokens

clean:
	cabal clean
	rm -f dragonlex lexer lexer.hs *.tokens *.o *.hi