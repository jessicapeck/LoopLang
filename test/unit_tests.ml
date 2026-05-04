open Alcotest


let read_file filename =
    let channel = open_in filename in
    let length = in_channel_length channel in
    let contents = really_input_string channel length in
    close_in channel;
    contents


(* compiler tests (all pipeline stages) *)
let tests = [
    ("Empty pattern", "empty_pattern");
    ("Empty pattern (with newlines)", "empty_pattern_with_newlines");
    ("Single row", "single_row");
    ("Row indicators", "row_indicators");
    ("Stitch multipliers", "stitch_multipliers");
    ("Integer variables", "integer_variables");
    ("Function definition", "function_definition");
    ("Multiple line function return", "multiple_line_function_return");
    ("If-else statement", "if_else_statement");
    ("If statement (no else)", "if_statement");
    ("Defining variables (all types)", "variable_definitions");
    ("Passing a stitch sequence argument", "stitch_seq_arg");
    ("Function call within a stitch sequence", "function_call_seq_item");
    ("Expressions as row numbers and multipliers", "row_num_and_mult_expr");
    ("Parentheses around expressions", "paren_around_expr");
    ("Newlines at the start and end of constructs", "newline_at_start_and_end");
    ("Nested rows", "nested_rows");
    ("Nested stitch sequences", "nested_stitch_seqs");
    ("Given row counts", "given_row_counts");
    ("Incorrect given row counts", "incorrect_given_row_counts");
    ("For-loop to generate rows", "for_loop_rows");
    ("For-loop to perform calculations", "for_loop_calculations");
    ("Comment on its own line", "comment_statement");
    ("Comment within a row", "comment_in_row");
    ("Stitch types", "stitch_types");
    ("Row range", "row_range");
    ("Variable definition in an if-else statement", "if_else_var_defs");
    ("Unreturned row in function body", "row_not_returned");
    ("Passing a row argument", "row_lit_arg");
    ("Assigning a variable to another variable", "assign_var_to_var");
    ("Generic function", "generic_function");
    ("Function overloading", "function_overloading");
    ("Sequence mirroring", "mirror");
    ("Function call as an argument", "arg_func_call");
    ("Arithemtic and boolean operations", "all_arithmetic_boolean_ops");
    ("Returning a function call", "return_func_call");
    ("Different orderings of stitch type and multiplier", "stitch_type_ordering");
    ("Multipliers of zero and one", "zero_one_multipliers");
    ("Row range in a variable", "row_range_var");
    ("Rows with row count and comment combinations", "count_comment_rows");
    ("Sphere", "sphere")
]


(* type checker error tests *)
let type_checker_error_tests = [
    ("Inconsistent type inference", "inconsistent_type_inference", Type_checker.TypeError "inconsistent type inference for parameter 'x'");
    ("Undefined function", "undefined_function", Type_checker.TypeError "undefined function: 'foo'");
    ("Incorrect number of arguments", "incorrect_number_of_arguments", Type_checker.TypeError "there are no functions named 'foo' that match the number and type of arguments provided");
    ("Incorrect argument types", "incorrect_argument_types", Type_checker.TypeError "there are no functions named 'foo' that match the number and type of arguments provided");
    ("Undefined variable", "undefined_variable", Type_checker.TypeError "undefined variable: 'seq'");
    ("Binary arithmetic operations", "binary_arithmetic_operations", Type_checker.TypeError "binary arithmetic operations expect Integer operands");
    ("Binary comparison operations", "binary_comparison_operations", Type_checker.TypeError "binary comparison operations expect Integer operands");
    ("Binary logial operations", "binary_logical_operations", Type_checker.TypeError "binary logical operations expect Boolean operands");
    ("Unary arithmetic operations", "unary_arithmetic_operations", Type_checker.TypeError "unary arithmetic operations expect an Integer operand");
    ("Unary logical operations", "unary_logical_operations", Type_checker.TypeError "unary logical operations expect a Boolean operand");
    ("Stitch multiplier", "stitch_multiplier", Type_checker.TypeError "stitch multiplier expression expects an Integer multiplier");
    ("Stitch sequence multiplier (number)", "stitch_sequence_multiplier_number", Type_checker.TypeError "stitch sequence multiplier expression expects an Integer multiplier");
    ("Stitch sequence multiplier (sequence)", "stitch_sequence_multiplier_seq", Type_checker.TypeError "variable 'myrow' expected to be a StitchSequence, but found RowList");
    ("Stitch sequence item", "stitch_seq_item", Type_checker.TypeError "function 'foo' expected to return a StitchSequence, but found Boolean");
    ("Row number", "row_number", Type_checker.TypeError "row number expects an Integer");
    ("Row content", "row_content", Type_checker.TypeError "variable 'z' expected to be a StitchSequence, but found Integer");
    ("If-else statement condition", "if_else_condition", Type_checker.TypeError "if-else statement condition expected to be a Boolean");
    ("Row count", "row_count", Type_checker.TypeError "row count expects an Integer");
    ("Antisymmetric variable definition", "if_else_antisymmetric_var_def", Type_checker.TypeError "undefined variable: 'seq1'");
    ("Variable definition outside of function body", "out_of_scope_var", Type_checker.TypeError "undefined variable: 'seq1'");
    ("Function without a return statement", "no_return", Type_checker.TypeError "function 'foo' does not return a value");
    ("Inconsistent return types", "inconsistent_return_types", Type_checker.TypeError "function 'foo' has inconsistent return types");
    ("For-loop upper bound", "for_loop_upper", Type_checker.TypeError "upper bound of for-loop expects an Integer");
    ("For-loop lower bound", "for_loop_lower", Type_checker.TypeError "lower bound of for-loop expects an Integer");
    ("Expected stitch sequence variable", "expected_stitch_seq_var", Type_checker.TypeError "variable 'seq' expected to be a StitchSequence, but found Boolean");
    ("Expected row list variable", "expected_row_list_var", Type_checker.TypeError "variable 'row_list' expected to be a RowList, but found Integer")
]


(* row number error tests *)
let row_number_error_tests = [
    ("Literal integer row number", "literal_int_row_num", Backend.RowNumberError "expected row number 3, but found row number 4 in its place");
    ("Calculated row number", "calculated_row_num", Backend.RowNumberError "expected row number 3, but found row number 10 in its place");
    ("Row from function", "row_from_func", Backend.RowNumberError "expected row number 2, but found row number 3 in its place");
    ("Not starting from one", "not_starting_from_one", Backend.RowNumberError "expected row number 1, but found row number 2 in its place");
    ("Row number range not increasing", "range_not_inc", Backend.RowNumberError "lower bound row number (3) should be strictly less than upper bound row number (1)");
    ("Incorrect lower bound row number", "lower_row_num", Backend.RowNumberError "expected row number 3, but found row number 4 in its place")
]


(* stitch error tests *)
let stitch_error_tests = [
    ("No chains in R1", "no_chains", Backend.StitchError "R1 of the pattern can only contain chain stitches or a magic ring");
    ("Chains and other stitches in R1", "chains_and_other_stitches", Backend.StitchError "R1 of the pattern can only contain chain stitches or a magic ring");
    ("Magic ring not in R1", "magic_ring", Backend.StitchError "magic rings are only valid in R1 of the pattern")
]


(* row count error tests *)
let row_count_error_tests = [
    ("Increase stitch", "inc_stitch", Backend.RowCountError "row number 3 is built on top of 5 stitches which is inconsistent with the previous row count of 10");
    ("Decrease stitch", "dec_stitch", Backend.RowCountError "row number 4 is built on top of 9 stitches which is inconsistent with the previous row count of 8");
    ("Rows from function", "rows_from_function", Backend.RowCountError "row number 2 is built on top of 8 stitches which is inconsistent with the previous row count of 5")
]


(* for loop error tests *)
let for_loop_error_tests = [
    ("For-loop bounds", "for_loop_bounds", Backend.ForLoopError "the for-loop expects the lower bound to be less than or equal to the upper bound, but found a lower bound of 5 and an upper bound of 1 being used")
]


(* divide by zero error tests *)
let divide_by_zero_error_tests = [
    ("Division by zero", "divide_by_zero", Backend.DivideByZeroError "division by zero encountered during expression evaluation")
]


(* TEST SUITES *)

let create_token_stream_test (test_name, filename) = 
    let test_fn () =
        let expected_token_stream = String.split_on_char '\n' (read_file ("./test/lexer_results/" ^ filename ^ ".tokens")) in
        let actual_token_stream = Test_utils.convert_to_token_stream ("./test/patterns/" ^ filename ^ ".loopy") in
        Alcotest.(check (list string)) test_name expected_token_stream actual_token_stream
    in
    Alcotest.test_case test_name `Quick test_fn

let token_stream_test_suite =
    List.map create_token_stream_test tests


let create_ast_test (test_name, filename) = 
    let test_fn () =
        let expected_ast = read_file ("./test/parser_results/" ^ filename ^ ".ast") in
        let actual_ast = Test_utils.convert_to_ast ("./test/patterns/" ^ filename ^ ".loopy") in
        Alcotest.(check string) test_name expected_ast actual_ast
    in
    Alcotest.test_case test_name `Quick test_fn

let ast_test_suite =
    List.map create_ast_test tests


let create_type_checker_test (test_name, filename) =
    let test_fn () =
        try
            let _ = Test_utils.run_type_checker ("./test/patterns/" ^ filename ^ ".loopy") in
            Alcotest.(check unit) test_name () ()
        with
        | Type_checker.TypeError msg -> Alcotest.fail ("Unexpected TypeError: " ^ msg ^ "\n")
    in
    Alcotest.test_case test_name `Quick test_fn

let type_checker_test_suite =
    List.map create_type_checker_test tests


let create_type_checker_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_type_checker ("./test/error_patterns/type_checker_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let type_checker_error_test_suite =
    List.map create_type_checker_error_test type_checker_error_tests


let create_compiler_test (test_name, filename) = 
    let test_fn () =
        let expected_result = String.trim (read_file ("./test/compiler_results/" ^ filename ^ ".txt")) in
        let actual_result = Test_utils.compile ("./test/patterns/" ^ filename ^ ".loopy") in
        Alcotest.(check string) test_name expected_result actual_result
    in
    Alcotest.test_case test_name `Quick test_fn

let compiler_test_suite =
    List.map create_compiler_test tests


let create_row_number_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_backend ("./test/error_patterns/row_number_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let row_number_error_test_suite =
    List.map create_row_number_error_test row_number_error_tests


let create_stitch_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_backend ("./test/error_patterns/stitch_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let stitch_error_test_suite =
    List.map create_stitch_error_test stitch_error_tests


let create_row_count_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_backend ("./test/error_patterns/row_count_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let row_count_error_test_suite =
    List.map create_row_count_error_test row_count_error_tests


let create_for_loop_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_backend ("./test/error_patterns/for_loop_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let for_loop_error_test_suite =
    List.map create_for_loop_error_test for_loop_error_tests


let create_divide_by_zer_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_backend ("./test/error_patterns/divide_by_zero_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let divide_by_zero_error_test_suite =
    List.map create_divide_by_zer_error_test divide_by_zero_error_tests


(* RUN TESTS *)

let () =
    let test_suites = [
        ("Token Stream Conversion Test", token_stream_test_suite);
        ("AST Conversion Test", ast_test_suite);
        ("Type Checker Test", type_checker_test_suite);
        ("Type Checker Error Test", type_checker_error_test_suite);
        ("Compiled Pattern", compiler_test_suite);
        ("Row Number Error Test", row_number_error_test_suite);
        ("Stitch Error Test", stitch_error_test_suite);
        ("Row Count Error Test", row_count_error_test_suite);
        ("For-Loop Error Test", for_loop_error_test_suite);
        ("Divide By Zero Error Test", divide_by_zero_error_test_suite)
    ] in
    run "LoopLang Compiler" test_suites
