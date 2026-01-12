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
let error_tests = [
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


let create_token_stream_test (test_name, filename) = 
    let test_fn () =
        let expected_token_stream = String.split_on_char '\n' (read_file ("./test/lexer_results/" ^ filename ^ ".tokens")) in
        let actual_token_stream = Test_utils.convert_to_token_stream ("./test/patterns/" ^ filename ^ ".txt") in
        Alcotest.(check (list string)) test_name expected_token_stream actual_token_stream
    in
    Alcotest.test_case test_name `Quick test_fn

let token_stream_test_suite =
    List.map create_token_stream_test tests


let create_ast_test (test_name, filename) = 
    let test_fn () =
        let expected_ast = read_file ("./test/parser_results/" ^ filename ^ ".ast") in
        let actual_ast = Test_utils.convert_to_ast ("./test/patterns/" ^ filename ^ ".txt") in
        Alcotest.(check string) test_name expected_ast actual_ast
    in
    Alcotest.test_case test_name `Quick test_fn

let ast_test_suite =
    List.map create_ast_test tests


let create_type_checker_test (test_name, filename) =
    let test_fn () =
        try
            let _ = Test_utils.run_type_checker ("./test/patterns/" ^ filename ^ ".txt") in
            Alcotest.(check unit) test_name () ()
        with
        | Type_checker.TypeError msg -> Alcotest.fail ("Unexpected TypeError: " ^ msg ^ "\n")
    in
    Alcotest.test_case test_name `Quick test_fn

let type_checker_test_suite =
    List.map create_type_checker_test tests


let create_type_checker_error_test (test_name, filename, expected_error) =
    let test_fn () =
        Alcotest.check_raises test_name expected_error (fun () -> Test_utils.run_type_checker ("./test/error_patterns/" ^ filename ^ ".txt"))
    in
    Alcotest.test_case test_name `Quick test_fn

let type_checker_error_test_suite =
    List.map create_type_checker_error_test error_tests

let () =
    let test_suites = [
        ("Pattern -> Token Stream Conversion Test", token_stream_test_suite);
        ("Pattern -> AST Conversion Test", ast_test_suite);
        ("Pattern -> Type Checker Test", type_checker_test_suite);
        ("Pattern -> Type Checker Error Test", type_checker_error_test_suite)
    ] in
    run "LoopLang Compiler" test_suites
