# LoopLang: A Language for Crochet Patterns

## Welcome to LoopLang!

LoopLang is a domain-specific language for crochet patterns.

Despite traditionally being viewed as a purely artistic hobby, there are strong links between crochet and programming. LoopLang has been designed to exploit the similarities between crochet and programming to enable people to write crochet patterns more efficiently and with fewer mistakes.

Navigate to the [documentation.md](documentation.md) file to learn about the LoopLang syntax!

## The Compiler

The LoopLang compiler translates LoopLang programs into traditional crochet patterns. The compiler also performs crochet-specific tasks, including:
- adding stitch counts to the end of each row
- ensuring the structural correctess of crochet patterns by verifying that neighbouring stitch count relationships are consistent
- verifying that only certain stitch types appear in row 1

## The Website

Play around with the LoopLang compiler online at [jessicapeck.github.io/LoopLang](https://jessicapeck.github.io/LoopLang/)!

## Setup and Execution

### Makefile Rules
```
>> make help
Usage: make [target]

Targets:
  help            Show help
  all             Build the compilers and the unit test executable
  compiler        Build the ./loopycompiler executable
  web-compiler    Build the ./web/public/loopycompiler.js file
  test            Build the ./test/unit_tests executable
  clean           Remove all generated files
  patterns-clean  Remove all compiled results from the ./test/patterns/ directory
  coverage        Run tests with coverage tracking and generate reports
```

After you have downloaded the repository, run the following commands before attempting to use the LoopLang compiler:

```
eval $(opam env)
make clean
make all
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

### Requirements

```
# Name               # Installed # Synopsis
alcotest             1.9.1       Alcotest is a lightweight and colourful test framework
bisect_ppx           2.8.3       Code coverage for OCaml
js_of_ocaml          6.2.0       Compiler from OCaml bytecode to JavaScript
js_of_ocaml-ppx      6.2.0       Compiler from OCaml bytecode to JavaScript
menhir               20250912    An LR(1) parser generator
ocaml                4.14.1      The OCaml compiler (virtual package)
```

