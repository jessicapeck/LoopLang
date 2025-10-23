open Lexing
open Ast

let () =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.pattern Lexer.token lexbuf in
    print_endline (Ast.string_of_expr ast)
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      exit 1
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1