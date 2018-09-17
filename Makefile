CABAL=cabal
GHC=ghc

.PHONY: gen build-cabal build-ghc run-cabal run-ghc clean

gen:
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

build-cabal: gen
	$(CABAL) build

build-ghc: gen
	$(GHC) -isrc src/Main.hs -o Main

run-cabal:
	$(CABAL) run

run-ghc: build-ghc
	./Main

clean:
	rm -rf src/*.hi src/*.o
	rm -rf dist src/Lexer.hs src/Parser.hs
	rm -rf Main
