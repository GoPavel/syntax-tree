CABAL=cabal

.PHONY: gen build run clean run-fresh

gen:
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

build: gen
	$(CABAL) build

run-fresh: build
	$(CABAL) run

run:
	$(CABAL) run

clean:
	rm -rf dist src/Lexer.hs src/Parser.hs
