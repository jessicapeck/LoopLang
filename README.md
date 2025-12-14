# LoopLang: A Language for Crochet Patterns

## Setup and execution

### Makefile commands
- `make` to build the `./llcompiler` executable
- `make test` to build the `./test/unit_tests` executable
- `make clean` to remove all generated files

### Running the compiler
```
./llcompiler ./test/patterns/<filename>.txt
```

### Running unit tests
```
eval $(opam env)
./test/unit_tests --show-errors
```