# LoopLang: A Language for Crochet Patterns

## Setup and execution

### Makefile commands
- `make` to build the `./loopycompiler` executable
- `make test` to build the `./test/unit_tests` executable
- `make clean` to remove all generated files
- `make patterns-clean` to remove all compiled results from the `./test/patterns/` directory

### Running the compiler
Executing the following command will compile the given `.loopy` file into a `.txt` file of the same filename.
```
./loopycompiler ./test/patterns/<filename>.loopy
```

### Running unit tests
```
eval $(opam env)
./test/unit_tests --show-errors
```