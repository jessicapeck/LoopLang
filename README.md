# LoopLang: A Language for Crochet Patterns

## Welcome to LoopLang!

LoopLang is a domain-specific language for crochet patterns.

Despite traditionally being viewed as a purely artistic hobby, there are strong links between crochet and programming. LoopLang has been designed to exploit the similarities between crochet and programming to enable people to write crochet patterns more efficiently and with fewer mistakes.

Navigate to the [documentation.md](documentation.md) file to learn about the LoopLang syntax!

## The Compiler

The LoopLang language comes with a compiler which validates the LoopLang program and converts it into a traditional crochet pattern.

## The Website

Play around with the LoopLang compiler online at [jessicapeck.github.io/LoopLang](https://jessicapeck.github.io/LoopLang/)!

## Setup and Execution

### Makefile Rules
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

### Running the Compiler
Executing the following command will compile the given `.loopy` file into a `.txt` file of the same filename.
```
./loopycompiler ./test/patterns/<filename>.loopy
```

### Running Unit Tests
```
eval $(opam env)
./test/unit_tests --show-errors
```