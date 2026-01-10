open Alcotest
open Lexing
open Ast


let read_file filename =
    let channel = open_in filename in
    let length = in_channel_length channel in
    let contents = really_input_string channel length in
    close_in channel;
    contents


let convert_to_token_stream filename =
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let rec aux acc =
        let token = Lexer.next_token lexbuf in
        match token with
        | Parser.EOF -> List.rev (Lexer.string_of_token token :: acc)
        | _ -> aux (Lexer.string_of_token token :: acc)
    in
    let tokens = aux [] in
    close_in channel;
    tokens


let convert_to_ast filename =
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.pattern Lexer.next_token lexbuf in
    close_in channel;
    Ast.string_of_pattern ast


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
    ("Function call within a stitch sequence", "function_call_seq_item")
]


let create_token_stream_test (test_name, filename) = 
    let test_fn () =
        let expected_token_stream = String.split_on_char '\n' (read_file ("./test/lexer_results/" ^ filename ^ ".tokens")) in
        let actual_token_stream = convert_to_token_stream ("./test/patterns/" ^ filename ^ ".txt") in
        Alcotest.(check (list string)) test_name expected_token_stream actual_token_stream
    in
    Alcotest.test_case test_name `Quick test_fn

let token_stream_test_suite =
    List.map create_token_stream_test tests


let create_ast_test (test_name, filename) = 
    let test_fn () =
        let expected_ast = read_file ("./test/parser_results/" ^ filename ^ ".ast") in
        let actual_ast = convert_to_ast ("./test/patterns/" ^ filename ^ ".txt") in
        Alcotest.(check string) test_name expected_ast actual_ast
    in
    Alcotest.test_case test_name `Quick test_fn

let ast_test_suite =
    List.map create_ast_test tests


let () =
    let test_suites = [
        ("Pattern -> Token Stream Conversion Test", token_stream_test_suite)
        ("Pattern -> AST Conversion Test", ast_test_suite);
    ] in
    run "LoopLang Compiler" test_suites

