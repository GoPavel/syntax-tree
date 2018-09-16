
.PHONY: gen build run clean

gen:
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

build: gen
	cabal build

run-fresh: build
	cabal run

run:
	cabal run

clean:
	rm -rf dist src/Lexer.hs src/Parser.hs
