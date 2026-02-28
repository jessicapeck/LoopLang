OCAMLC = ocamlfind ocamlc
OCAMLLEX = ocamllex
MENHIR = menhir

SRC = src
TEST_DIR = test

TARGET = loopycompiler
TEST_EXEC = $(TEST_DIR)/unit_tests

ALCOTEST = alcotest
TEST_COVERAGE = bisect_ppx

ML_FILES = $(SRC)/ast.ml $(SRC)/parser.ml $(SRC)/lexer.ml $(SRC)/type_checker.ml $(SRC)/interpreter.ml
CMO_FILES = $(ML_FILES:.ml=.cmo)

MAIN_FILE = $(SRC)/looplang.ml
MAIN_CMO_FILE = $(MAIN_FILE:.ml=.cmo)

TEST_ML_FILES = $(TEST_DIR)/test_utils.ml $(TEST_DIR)/unit_tests.ml
TEST_CMO_FILES = $(TEST_ML_FILES:.ml=.cmo)

TEST_COVERAGE_DIR = _coverage_data
TEST_COVERAGE_DATA = $(TEST_COVERAGE_DIR)/bisect


ifeq ($(TRACK_COVERAGE), true)
	COVERAGE_FLAGS = -package $(TEST_COVERAGE)
else
	COVERAGE_FLAGS =
endif


# generate all files needed to create TARGET upon `make` command
all: $(TARGET)

# link everything together to create the final executable
$(TARGET): $(CMO_FILES) $(MAIN_CMO_FILE)
	$(OCAMLC) -g $(COVERAGE_FLAGS) -linkpkg -o $@ $^

# compile .ml files to .cmo files
$(SRC)/%.cmo: $(SRC)/%.ml
	$(OCAMLC) -g $(COVERAGE_FLAGS) -I $(SRC) -c -o $@ $<

# compile .mli files to .cmi files
$(SRC)/%.cmi: $(SRC)/%.mli
	$(OCAMLC) -I $(SRC) -c -o $@ $<

# generate parser.ml and parser.mli from parser.mly
$(SRC)/parser.ml $(SRC)/parser.mli: $(SRC)/parser.mly
	$(MENHIR) --ocamlc '$(OCAMLC) $(COVERAGE_FLAGS) -I $(SRC)' --infer --base $(SRC)/parser $<

# specify dependency for parser.cmo
$(SRC)/parser.cmo: $(SRC)/parser.cmi

# generate lexer.ml from lexer.mll
${SRC}/lexer.ml: ${SRC}/lexer.mll
	$(OCAMLLEX) -o $@ $<

test: $(TEST_EXEC)

$(TEST_EXEC): $(CMO_FILES) $(TEST_CMO_FILES) 
	opam exec -- $(OCAMLC) -g -linkpkg -o $@ -package $(ALCOTEST) $(COVERAGE_FLAGS) -I $(SRC) $^

# compile .ml files into object filed
$(TEST_DIR)/%.cmo: $(TEST_DIR)/%.ml
	opam exec -- $(OCAMLC) -g -I $(SRC) -I $(TEST_DIR) -package $(ALCOTEST) $(COVERAGE_FLAGS) -c -o $@ $<

# remove all generated files
clean:
	rm -f $(SRC)/*.cmi $(SRC)/*.cmo $(SRC)/*.cmx $(SRC)/lexer.ml $(SRC)/parser.ml $(SRC)/parser.mli $(TARGET) $(TEST_DIR)/*.cmi $(TEST_DIR)/*.cmo $(TEST_EXEC)

# remove all compiled results from the patterns folder
patterns-clean:
	rm -f test/patterns/*.txt

coverage:
	make clean
	make TRACK_COVERAGE=true
	make test TRACK_COVERAGE=true
	BISECT_FILE=$(TEST_COVERAGE_DATA) ./$(TEST_EXEC)
	make clean
	make
	make test
	@echo "\n\n---------- REPORT WEBSITE ----------"
	bisect-ppx-report html $(TEST_COVERAGE_DIR)/*.coverage
	@echo "Report generated in _coverage/index.html"
	@echo "---------- REPORT SUMMARY ----------"
	bisect-ppx-report summary --per-file $(TEST_COVERAGE_DIR)/*.coverage

.PHONY: all clean patterns-clean coverage
