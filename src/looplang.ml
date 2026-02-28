open Lexing
open Ast

let tokens = ref []

let log_lexer lexer_func lexbuf =
    let token = lexer_func lexbuf in
    tokens := token :: !tokens;
    token

let write_result_to_file filename result =
    let out_channel = open_out filename in
    List.iter (fun line ->
        Printf.fprintf out_channel "%s\n" line
    ) result;
    close_out out_channel

let green s = "\x1b[32m" ^ s ^ "\x1b[0m"
let red s = "\x1b[31m" ^ s ^ "\x1b[0m"

let print_boxed_error err_type err_msg =
    let width = max (String.length err_type) (String.length err_msg) in
    let border = red ("+-" ^ (String.make width '-') ^ "-+") in
    let pipe = red "|" in

    Printf.eprintf "%s\n" border;
    Printf.eprintf "%s %s%s %s\n" pipe err_type (String.make (width - String.length err_type) ' ') pipe;
    Printf.eprintf "%s %s%s %s\n" pipe err_msg (String.make (width - String.length err_msg) ' ') pipe;
    Printf.eprintf "%s\n" border

let () =
    let filename = Sys.argv.(1) in
    let in_channel = open_in filename in

    let ext = Filename.extension filename in
    if ext <> ".loopy" then raise (Failure "Error: input file must have .loopy extension");

    let lexbuf = Lexing.from_channel in_channel in
    let debug_lexer = log_lexer Lexer.next_token in
    try
        let ast = Parser.pattern debug_lexer lexbuf in
        (* print_endline (Ast.string_of_pattern ast) *)

        let _ = Type_checker.check_pattern ast in

        let result = Interpreter.eval_pattern ast in
        close_in in_channel;

        let name_no_ext = Filename.remove_extension filename in
        let output_filename = name_no_ext ^ ".txt" in
        write_result_to_file output_filename result;
        (* List.iter print_string result *)

        let success_msg = (green "Pattern compiled successfully! ") ^ (Printf.sprintf "Result written to %s" output_filename) in
        Printf.printf "%s\n" success_msg
    with
    | Parser.Error ->
        close_in in_channel;
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, column %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
        (* Printf.eprintf "--- Tokens seen up to error ---\n";
        List.iter (fun token ->
            Printf.eprintf "%s\n" (Lexer.string_of_token token)
        ) (List.rev !tokens); *)
        exit 1
    | Type_checker.TypeError msg ->
        close_in in_channel;
        print_boxed_error "TypeError" msg;
        exit 1
    | Interpreter.RowNumberError msg ->
        close_in in_channel;
        print_boxed_error "RowNumberError" msg;
        exit 1
    | Interpreter.RowOneError msg ->
        close_in in_channel;
        print_boxed_error "RowOneError" msg;
        exit 1
    | Interpreter.RowCountError msg ->
        close_in in_channel;
        print_boxed_error "RowCountError" msg;
        exit 1
    | Interpreter.ForLoopError msg ->
        close_in in_channel;
        print_boxed_error "ForLoopError" msg;
        exit 1
    | Failure msg ->
        close_in in_channel;
        Printf.eprintf "Error: %s\n" msg;
        exit 1
