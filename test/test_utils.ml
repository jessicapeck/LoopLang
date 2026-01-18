open Lexing
open Ast


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


let run_type_checker filename =
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.pattern Lexer.next_token lexbuf in
    close_in channel;
    let _ = Type_checker.check_pattern ast in
    ()
