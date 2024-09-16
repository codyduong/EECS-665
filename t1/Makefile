build:
	cabal exec ghc -- -o dragonlex dragonlex.hs

run:
	./dragonlex B.spec
	cabal exec ghc -- -o lexer lexer.hs

test:
	# ./lexer test1.txt > test1.tokens
	./lexer test2.txt

clean:
	rm -f dragonlex lexer lexer.hs *.tokens *.o *.hi