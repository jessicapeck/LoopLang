open Lexing
open Ast


(* helper functions for constructing the JSON string to return *)

let escape_json_str s =
    let b = Buffer.create (String.length s) in
    String.iter (function
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\r' -> Buffer.add_string b "\\r"
        | '\t' -> Buffer.add_string b "\\t"
        | c -> Buffer.add_char b c
    ) s;
    Buffer.contents b

let construct_json result warnings error_msg =
    let warnings_list = List.map (fun w -> Printf.sprintf "\"%s\"" (escape_json_str w)) warnings in
    let warnings_str = Printf.sprintf "[%s]" (String.concat ", " warnings_list) in
    let json_str = Printf.sprintf "{ \"result\": \"%s\", \"warnings\": %s, \"error\": \"%s\" }" (escape_json_str result) warnings_str (escape_json_str error_msg) in
    Printf.printf "%s\n" json_str

let construct_err_json error_msg =
    let warnings = !Interpreter.warning_messages in
    Interpreter.warning_messages := [];
    construct_json "" warnings error_msg


(* compilation logic *)

let () =
    let code_str = Sys.argv.(1) in
    let lexbuf = Lexing.from_string code_str in
    try
        let ast = Parser.pattern Lexer.next_token lexbuf in

        let _ = Type_checker.check_pattern ast in

        let result = Interpreter.eval_pattern ast in
        let combined_result = String.concat "\n" result in
        let warnings = List.rev !Interpreter.warning_messages in
        Interpreter.warning_messages := [];
        construct_json combined_result warnings ""

    with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        let msg = Printf.sprintf "syntax error at line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
        let error_msg = ("ParserError: " ^ msg) in
        construct_err_json error_msg
    | Type_checker.TypeError msg ->
        let error_msg = ("TypeError: " ^ msg) in
        construct_err_json error_msg
    | Interpreter.RowNumberError msg ->
        let error_msg = ("RowNumberError: " ^ msg) in
        construct_err_json error_msg
    | Interpreter.RowOneError msg ->
        let error_msg = ("RowOneError: " ^ msg) in
        construct_err_json error_msg
    | Interpreter.RowCountError msg ->
        let error_msg = ("RowCountError: " ^ msg) in
        construct_err_json error_msg
    | Interpreter.ForLoopError msg ->
        let error_msg = ("ForLoopError: " ^ msg) in
        construct_err_json error_msg
    | Failure msg ->
        let error_msg = ("Error: " ^ msg) in
        construct_err_json error_msg
