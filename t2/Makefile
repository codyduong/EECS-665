.PHONY: clean

HS_SOURCE=dragonsdt.hs
HS_PROGRAM=dragonsdt

CXX_PROGRAM=translator
CXX_SOURCE=translator.cpp

AG_INPUT=input.ag
TOKENS_INPUT=test.tokens

build: $(HS_SOURCE)
  # we can use ghc since we don't have any external deps
	ghc -o $(HS_PROGRAM) $(HS_SOURCE)

run: $(HS_PROGRAM) $(AG_INPUT)
	./$(HS_PROGRAM) $(AG_INPUT)
	if [ -f $(CXX_SOURCE) ]; then \
		$(CXX) $(CXXFLAGS) -o $(CXX_PROGRAM) $(CXX_SOURCE); \
	fi

test: $(CXX_PROGRAM) $(TOKENS_INPUT)
	./$(CXX_PROGRAM) < $(TOKENS_INPUT)

clean:
	rm -f $(HS_PROGRAM) $(HS_PROGRAM).hi $(HS_PROGRAM).o
	rm -f $(CXX_PROGRAM) $(CXX_SOURCE)
