open Lexing
open Ast

let tokens = ref []

let log_lexer lexer_func lexbuf =
  let token = lexer_func lexbuf in
  tokens := token :: !tokens;
  token

let () =
  let filename = Sys.argv.(1) in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in

  let debug_lexer = log_lexer Lexer.next_token in
  try
    let ast = Parser.pattern debug_lexer lexbuf in
    (* print_endline (Ast.string_of_pattern ast) *)

    let initial_env = [] in
    let _ = Type_checker.check_pattern initial_env ast in
    Printf.printf("Type checking: SUCCESS")
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);

      Printf.eprintf "--- Tokens seen up to error ---\n";
      List.iter (fun token ->
        Printf.eprintf "%s\n" (Lexer.string_of_token token)
      ) (List.rev !tokens);
      exit 1
  | Type_checker.TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      exit 1
  | Type_checker.ArgError msg ->
      Printf.eprintf "Argument error: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1