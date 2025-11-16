{
    open Parser
    open Lexing

    let indent_stack = Stack.create ()
    let () = Stack.push 0 indent_stack

    let pending_tokens = Queue.create ()

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { 
            pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1 
        }

    let calc_indent_width seq = 
        String.fold_left (fun acc c ->
            match c with
            | ' ' -> acc + 1
            | '\t' -> acc + 4
            | _ -> acc
        ) 0 seq
    
    let process_indentation seq lexbuf = 
        (* only consider whitespace sequence as indentation if at the start of a line *)
        if Lexing.bol lexbuf then
            let current_indent_width = calc_indent_width seq
            if Stack.is_empty indent_stack then
                Stack.push current_indent_width indent_stack
            else
                let prev_indent_count = Stack.top indent_stack in
                if current_indent_width > prev_indent_count then
                    Stack.push current_indent_width indent_stack;
                    INDENT
                else if current_indent_width < prev_indent_count then
                    let temp_token_queue = Queue.create () in
                    while not (Stack.is_empty indent_stack) and (current_indent_width < Stack.top indent_stack) do
                        Stack.pop indent_stack
                        Queue.add DEDENT temp_token_queue
                    done;
                    Queue.iter (fun t -> Queue.add t pending_tokens) temp_token_queue;
                    Queue.take temp_token_queue
                else token lexbuf
        else token lexbuf
}

let id_regex = ['a'-'z' 'A'-'Z' '0'-'9']+
let num_regex = ['0'-'9']+
let row_identifiers = ('r' | 'R' | "row" | "ROW")

rule token = parse
    | ' '+ as seq                                       { process_indentation indent_seq lexbuf }
    | '\t'+ as seq                                      { process_indentation indent_seq lexbuf }
    | '\n'                                              { next_line lexbuf; NEWLINE}
    | "ch"                                              { CH }
    | "sc"                                              { SC }
    | "dc"                                              { DC }
    | "inc"                                             { INC }
    | "dec"                                             { DEC }
    | row_identifiers ' '? num_regex                    { ROWINT }
    | row_identifiers ' ' (id_regex as id)              { ROWINTVAR id }
    | row_identifiers ' '? "(" (id_regex as id) ")"     { ROWINTVAR id }
    | 'x' (num_regex as num)                            { MULINT (int_of_string num)}
    | "x(" (id_regex as id) ")"                         { MULINTVAR id }
    | "let"                                             { LET }
    | "def"                                             { DEF }
    | "for"                                             { FOR }
    | "to"                                              { TO }
    | num_regex as num                                  { INT (int_of_string num) }
    | id_regex as id                                    { ID id }
    | '='                                               { EQ }
    | '('                                               { LPAREN }
    | ')'                                               { RPAREN }
    | '['                                               { LBRACKET }
    | ']'                                               { RBRACKET }
    | '{'                                               { LBRACE }
    | '}'                                               { RBRACE }
    | ':'                                               { COLON }
    | ','                                               { COMMA }
    | eof                                               {
                                                            while not (Stack.is_empty indent_stack) do
                                                                Stack.pop indent_stack
                                                                Queue.add DEDENT pending_tokens
                                                            done
                                                            Queue.add EOF pending_tokens;
                                                            Queue.take pending_tokens
                                                        }
    | _                                                 { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

let next_token lexbuf =
    if not (Queue.is_empty pending_tokens) then
        Queue.take pending_tokens
    else
        token lexbuf