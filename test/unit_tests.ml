open Alcotest
open Lexing
open Ast

let convert_to_ast filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let ast = Parser.pattern Lexer.next_token lexbuf in
  Ast.string_of_pattern ast


let () =
  print_endline (convert_to_ast "./test/patterns/integer_variables.txt");
