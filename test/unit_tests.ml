open Alcotest
open Lexing
open Ast


let read_file filename =
  let channel = open_in filename in
  let length = in_channel_length channel in
  let contents = really_input_string channel length in
  close_in channel;
  contents


let convert_to_ast filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let ast = Parser.pattern Lexer.next_token lexbuf in
  close_in channel;
  Ast.string_of_pattern ast


let convert_to_token_stream filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let rec aux acc =
    let token = Lexer.next_token lexbuf in
    match token with
    | Parser.EOF -> List.rev (token :: acc)
    | _ -> aux (token :: acc)
  in
  let tokens = aux [] in
  close_in channel;
  tokens


(* list of (test_name, filename) pairs *)
let tests = [
  ("Single row", "single_row");
  ("Row indicators", "row_indicators");
  ("Stitch multipliers", "stitch_multipliers");
  ("Integer variables", "integer_variables");
  ("Function definition", "function_definition")
]

let string_equality_test test_name expected actual = 
  Alcotest.(check string) test_name expected actual

let () =

  let tokens = convert_to_token_stream "./test/patterns/single_row.txt" in
  List.iter (fun token ->
    let token_string = Lexer.string_of_token token in
    Printf.printf "%s\n" token_string
  ) tokens;

  (* let test_cases = (List.map (fun (test_name, filename) ->
    let test_fn () =
      let expected = read_file ("./test/ast_results/" ^ filename ^ ".ast") in
      let actual = convert_to_ast ("./test/patterns/" ^ filename ^ ".txt") in
      string_equality_test test_name expected actual
    in
    (test_name, `Quick, test_fn)
  ) tests)
  in
  run "LoopLang Compiler" [("Pattern -> AST Conversion Test", test_cases)] *)
