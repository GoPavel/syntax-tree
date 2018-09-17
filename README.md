# Build
## Tool

- `alex`, `happy`
- `ghc` or `cabal`

## Build and Run with Cabal

```bash
$ make build-cabal
$ make run-cabal
```

## Build and Run with GHC

```bash
$ make build-ghc
$ make run-ghc
```

# Implemented syntax

It's part of syntax from C/C++.

- Type: `int`, `void`
- Math operation: `+`, `-`, `*`, `\`
- Comparing: `==`, `<`, `>`
- branch: `if {...} else {...}`
- Definition function: `type f(...) {...}`
  - return expression: `return 5 + f();`
- definition variable: `int x = ...;`
- declaration variable: `int x;`
- assignment: `x = ...;`
- digital value: `123432`

For example: [code.cpp](https://github.com/GoPavel/syntax-tree/blob/master/code.cpp)

## Grammar

```
Start
  : Code

Code
  : DefFunc Code
  | DefFunc

DefFunc
  : Type NAME "(" ArgsInDef ")" "{" SeqStatement "}"

ArgsInDef
  : DefVar "," ArgsInDef
  | DefVar
  | {- empty -}

Statement
  : Expr
  | DefVar
  | "return" Expr
  | NAME "=" Expr
  | "if" "(" Condition ")" "{" SeqStatement "}" "else" "{" SeqStatement "}"

DefVar
  : Type NAME
  | Type NAME "=" Expr

SeqStatement
  : Statement ";" SeqStatement
  | Statement
  | {- empty -}

Condition
  : Expr "==" Expr
  | Expr "<" Expr
  | Expr ">" Expr

Type
  : "int"
  | "void"

Expr
  : NAME "(" ArgsInCall ")"
  | "(" Expr ")"
  | Expr "+" Expr
  | Expr "-" Expr
  | Expr "*" Expr
  | Expr "\" Expr
  | NAME
  | VALUE

ArgsInCall
  : {- empty -}
  | Expr
  | Expr "," ArgsInCall
```
