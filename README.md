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
