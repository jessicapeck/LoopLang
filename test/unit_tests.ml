open Alcotest


let read_file filename =
    let channel = open_in filename in
    let length = in_channel_length channel in
    let contents = really_input_string channel length in
    close_in channel;
    contents


(* list of (test_name, filename) pairs *)
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
    ("Newlines at the start and end of constructs", "newline_at_start_and_end")
]


(* list of (test_name, filename, expected_error) *)
let type_checker_error_tests = [
    ("Inconsistent type inference", "inconsistent_type_inference", Type_checker.TypeError "inconsistent type inference for parameter 'x'");
    ("Undefined function", "undefined_function", Type_checker.TypeError "undefined function: 'foo'");
    ("Incorrect number of arguments", "incorrect_number_of_arguments", Type_checker.TypeError "number of arguments passed and number of parameters expected do not match");
    ("Incorrect argument types", "incorrect_argument_types", Type_checker.TypeError "argument types do not match expected parameter types");
    ("Undefined variable", "undefined_variable", Type_checker.TypeError "undefined variable: 'seq'");
    ("Binary arithmetic operations", "binary_arithmetic_operations", Type_checker.TypeError "binary arithmetic operations expect TInt operands");
    ("Binary comparison operations", "binary_comparison_operations", Type_checker.TypeError "binary comparison operations expect TInt operands");
    ("Binary logial operations", "binary_logical_operations", Type_checker.TypeError "binary logical operations expect TBool operands");
    ("Unary arithmetic operations", "unary_arithmetic_operations", Type_checker.TypeError "unary arithmetic operations expect a TInt operand");
    ("Unary logical operations", "unary_logical_operations", Type_checker.TypeError "unary logical operations expect a TBool operand");
    ("Stitch multiplier", "stitch_multiplier", Type_checker.TypeError "stitch multiplier expression expects TInt");
    ("Stitch sequence multiplier (number)", "stitch_sequence_multiplier_number", Type_checker.TypeError "stitch sequence multiplier expression expects TInt");
    ("Stitch sequence multiplier (sequence)", "stitch_sequence_multiplier_seq", Type_checker.TypeError "variable 'myrow' expected TStitchSeq, but found TRowList");
    ("Stitch sequence item", "stitch_seq_item", Type_checker.TypeError "function 'foo' expected to return TStitchSeq, but found TBool");
    ("Row number", "row_number", Type_checker.TypeError "row number expects TInt");
    ("Row content", "row_content", Type_checker.TypeError "variable 'z' expected TStitchSeq, but found TInt");
    ("If-else statement condition", "if_else_condition", Type_checker.TypeError "if-else statement condition expects TBool")
]


let row_number_error_tests = [
    ("Literal integer row number", "literal_int_row_num", Interpreter.RowNumberError "expected row number 3, but found row number 4 in its place");
    ("Calculated row number", "calculated_row_num", Interpreter.RowNumberError "expected row number 3, but found row number 10 in its place");
    ("Row from function", "row_from_func", Interpreter.RowNumberError "expected row number 2, but found row number 3 in its place");
    ("Not starting from one", "not_starting_from_one", Interpreter.RowNumberError "expected row number 1, but found row number 2 in its place")
]


let row_one_error_tests = [
    ("No chains in R1", "no_chains", Interpreter.RowOneError "R1 of the pattern can only contain chain stitches");
    ("Chains and other stitches in R1", "chains_and_other_stitches", Interpreter.RowOneError "R1 of the pattern can only contain chain stitches")
]


let row_count_error_tests = [
    ("Increase stitch", "inc_stitch", Interpreter.RowCountError "row number 3 is built on top of 5 stitches which is inconsistent with the previous row count of 10");
    ("Decrease stitch", "dec_stitch", Interpreter.RowCountError "row number 4 is built on top of 9 stitches which is inconsistent with the previous row count of 8");
    ("Rows from function", "rows_from_function", Interpreter.RowCountError "row number 2 is built on top of 8 stitches which is inconsistent with the previous row count of 5")
]


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
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_interpreter ("./test/error_patterns/row_number_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let row_number_error_test_suite =
    List.map create_row_number_error_test row_number_error_tests


let create_row_one_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_interpreter ("./test/error_patterns/row_one_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let row_one_error_test_suite =
    List.map create_row_one_error_test row_one_error_tests


let create_row_count_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_interpreter ("./test/error_patterns/row_count_errors/" ^ filename ^ ".loopy"))
    in
    Alcotest.test_case test_name `Quick test_fn

let row_count_error_test_suite =
    List.map create_row_count_error_test row_count_error_tests


let () =
    let test_suites = [
        ("Token Stream Conversion Test", token_stream_test_suite);
        ("AST Conversion Test", ast_test_suite);
        ("Type Checker Test", type_checker_test_suite);
        ("Type Checker Error Test", type_checker_error_test_suite);
        ("Compiled Pattern", compiler_test_suite);
        ("Row Number Error Test", row_number_error_test_suite);
        ("Row One Error Test", row_one_error_test_suite);
        ("Row Count Error Test", row_count_error_test_suite)
    ] in
    run "LoopLang Compiler" test_suites
