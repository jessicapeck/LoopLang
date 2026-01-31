open Lexing
open Ast


let convert_to_token_stream filename =
    let channel = open_in filename in
    try
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
    with e ->
        close_in channel;
        raise e


let convert_to_ast filename =
    let channel = open_in filename in
    try
        let lexbuf = Lexing.from_channel channel in
        let ast = Parser.pattern Lexer.next_token lexbuf in
        close_in channel;
        Ast.string_of_pattern ast
    with e ->
        close_in channel;
        raise e


let run_type_checker filename =
    let channel = open_in filename in
    try
        let lexbuf = Lexing.from_channel channel in
        let ast = Parser.pattern Lexer.next_token lexbuf in
        close_in channel;
        let _ = Type_checker.check_pattern ast in
        ()
    with e ->
        close_in channel;
        raise e


let compile filename =
    let channel = open_in filename in
    try
        let lexbuf = Lexing.from_channel channel in
        let ast = Parser.pattern Lexer.next_token lexbuf in
        let _ = Type_checker.check_pattern ast in
        let result = Interpreter.eval_pattern ast in
        close_in channel;
        String.concat "\n" result
    with e ->
        close_in channel;
        raise e

let run_interpreter filename =
    let channel = open_in filename in
    try
        let lexbuf = Lexing.from_channel channel in
        let ast = Parser.pattern Lexer.next_token lexbuf in
        let _ = Type_checker.check_pattern ast in
        let _ = Interpreter.eval_pattern ast in
        close_in channel;
        ()
    with e ->
        close_in channel;
        raise e