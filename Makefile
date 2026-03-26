OCAMLC = ocamlfind ocamlc
OCAMLLEX = ocamllex
MENHIR = menhir

SRC = src
TEST_DIR = test
WEB_DIR = web/public

TARGET = loopycompiler
TARGET_WEB = $(WEB_DIR)/loopycompiler.js
TEST_EXEC = $(TEST_DIR)/unit_tests

ALCOTEST = alcotest
TEST_COVERAGE = bisect_ppx
JSOO = js_of_ocaml
JSOO_PPX = js_of_ocaml-ppx

ML_FILES = $(SRC)/ast.ml $(SRC)/parser.ml $(SRC)/lexer.ml $(SRC)/type_checker.ml $(SRC)/interpreter.ml
CMO_FILES = $(ML_FILES:.ml=.cmo)

MAIN_FILE = $(SRC)/looplang.ml
MAIN_CMO_FILE = $(MAIN_FILE:.ml=.cmo)

MAIN_FILE_WEB = $(SRC)/looplang_web.ml
MAIN_CMO_FILE_WEB = $(MAIN_FILE_WEB:.ml=.cmo)

TEST_ML_FILES = $(TEST_DIR)/test_utils.ml $(TEST_DIR)/unit_tests.ml
TEST_CMO_FILES = $(TEST_ML_FILES:.ml=.cmo)

TEST_COVERAGE_DIR = _coverage_data
TEST_COVERAGE_DATA = $(TEST_COVERAGE_DIR)/bisect


ifeq ($(TRACK_COVERAGE), true)
	COVERAGE_PACKAGES = -package $(TEST_COVERAGE)
else
	COVERAGE_PACKAGES =
endif

ifeq ($(WEB_MODE), true)
	JSOO_PACKAGES = -package $(JSOO) -package $(JSOO_PPX)
else
	JSOO_PACKAGES =
endif

help: ## Show help
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@awk 'BEGIN {FS = ":.*?## "}; /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

all: ## Build the compiler and unit test executables
	make compiler
	make web-compiler
	make test

compiler: $(TARGET) ## Build the ./loopycompiler executable

# link everything together to create the final executable
$(TARGET): $(CMO_FILES) $(MAIN_CMO_FILE)
	$(OCAMLC) -g $(COVERAGE_PACKAGES) $(JSOO_PACKAGES) -linkpkg -o $@ $^

web-compiler: ## Build the ./web/public/loopycompiler.js file
	make $(TARGET_WEB) WEB_MODE=true

# link everything together to create the final executable
$(TARGET_WEB): $(CMO_FILES) $(MAIN_CMO_FILE_WEB)
	$(OCAMLC) -linkpkg -o $(WEB_DIR)/temp_out.byte -package $(JSOO) -package $(JSOO_PPX) $^
	$(JSOO) $(WEB_DIR)/temp_out.byte -o $(TARGET_WEB)
	rm $(WEB_DIR)/temp_out.byte

# compile .ml files to .cmo files
$(SRC)/%.cmo: $(SRC)/%.ml
	$(OCAMLC) -g $(COVERAGE_PACKAGES) $(JSOO_PACKAGES) -I $(SRC) -c -o $@ $<

# compile .mli files to .cmi files
$(SRC)/%.cmi: $(SRC)/%.mli
	$(OCAMLC) -I $(SRC) -c -o $@ $<

# generate parser.ml and parser.mli from parser.mly
$(SRC)/parser.ml $(SRC)/parser.mli: $(SRC)/parser.mly
	$(MENHIR) --ocamlc '$(OCAMLC) $(COVERAGE_PACKAGES) -I $(SRC)' --infer --base $(SRC)/parser $<

# specify dependency for parser.cmo
$(SRC)/parser.cmo: $(SRC)/parser.cmi

# generate lexer.ml from lexer.mll
${SRC}/lexer.ml: ${SRC}/lexer.mll
	$(OCAMLLEX) -o $@ $<

test: $(TEST_EXEC) ## Build the ./test/unit_tests executable

$(TEST_EXEC): $(CMO_FILES) $(TEST_CMO_FILES) 
	opam exec -- $(OCAMLC) -g -linkpkg -o $@ -package $(ALCOTEST) $(COVERAGE_PACKAGES) -I $(SRC) $^

# compile .ml files into object filed
$(TEST_DIR)/%.cmo: $(TEST_DIR)/%.ml
	opam exec -- $(OCAMLC) -g -I $(SRC) -I $(TEST_DIR) -package $(ALCOTEST) $(COVERAGE_PACKAGES) -c -o $@ $<

clean: ## Remove all generated files
	rm -f $(SRC)/*.cmi $(SRC)/*.cmo $(SRC)/*.cmx $(SRC)/lexer.ml $(SRC)/parser.ml $(SRC)/parser.mli $(TARGET) $(TEST_DIR)/*.cmi $(TEST_DIR)/*.cmo $(TEST_EXEC) $(TARGET_WEB)

patterns-clean: ## Remove all compiled results from the ./test/patterns/ directory
	rm -f test/patterns/*.txt

coverage: ## Run tests with coverage tracking and generate reports
	make clean
	make compiler TRACK_COVERAGE=true
	make test TRACK_COVERAGE=true
	BISECT_FILE=$(TEST_COVERAGE_DATA) ./$(TEST_EXEC)
	make clean
	make all
	@echo "\n\n---------- REPORT WEBSITE ----------"
	bisect-ppx-report html $(TEST_COVERAGE_DIR)/*.coverage
	@echo "Report generated in _coverage/index.html"
	@echo "---------- REPORT SUMMARY ----------"
	bisect-ppx-report summary --per-file $(TEST_COVERAGE_DIR)/*.coverage

.PHONY: help all compiler web-compiler test clean patterns-clean coverage
