# LoopLang: A Language for Crochet Patterns

## Setup and execution

### Makefile commands
```
> make help
Usage: make [target]

Targets:
  help            Show help
  all             Build the compiler and unit test executables
  compiler        Build the ./loopycompiler executable
  web-compiler    Build the ./web/public/loopycompiler.js file
  test            Build the ./test/unit_tests executable
  clean           Remove all generated files
  patterns-clean  Remove all compiled results from the ./test/patterns/ directory
  coverage        Run tests with coverage tracking and generate reports
```

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