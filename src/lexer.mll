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

    type indent_action = 
    | IndentToken
    | DedentToken
    | Skip

    let process_indentation seq lexbuf = 
        (* only consider whitespace sequence as indentation if at the start of a line *)
        if lexbuf.lex_start_p.pos_cnum = lexbuf.lex_start_p.pos_bol then
            begin
                let current_indent_width = calc_indent_width seq in
                if Stack.is_empty indent_stack then
                    begin
                        Stack.push current_indent_width indent_stack; 
                        Skip
                    end
                else
                    begin
                        let prev_indent_count = Stack.top indent_stack in
                        if current_indent_width > prev_indent_count then
                            begin
                                Stack.push current_indent_width indent_stack;
                                IndentToken
                            end
                        else if current_indent_width < prev_indent_count then
                            begin
                                let temp_token_queue = Queue.create () in
                                while not (Stack.is_empty indent_stack) && (current_indent_width < Stack.top indent_stack) do
                                    let _ = Stack.pop indent_stack in
                                    Queue.add DEDENT temp_token_queue
                                done;
                                Queue.iter (fun t -> Queue.add t pending_tokens) temp_token_queue;
                                let _ = Queue.take pending_tokens in
                                DedentToken
                            end
                        else Skip
                    end
            end
        else Skip
}

let id_regex = ['a'-'z' 'A'-'Z' '0'-'9']+
let num_regex = ['0'-'9']+
let row_identifiers = ('r' | 'R' | "row" | "ROW")

rule token = parse
    | (' '+) | ('\t'+) as seq                           { match process_indentation seq lexbuf with
                                                            | IndentToken -> INDENT
                                                            | DedentToken -> DEDENT
                                                            | Skip -> token lexbuf
                                                        }
    | '\n'                                              { next_line lexbuf; NEWLINE}
    | "ch"                                              { CH }
    | "sc"                                              { SC }
    | "dc"                                              { DC }
    | "inc"                                             { INC }
    | "dec"                                             { DEC }
    | row_identifiers ' '? (num_regex as num)           { ROWINT (int_of_string num) }
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
                                                            begin
                                                                while not (Stack.is_empty indent_stack) do
                                                                    let _ = Stack.pop indent_stack in
                                                                    Queue.add DEDENT pending_tokens
                                                                done;
                                                                Queue.add EOF pending_tokens;
                                                                Queue.take pending_tokens
                                                            end
                                                        }
    | _                                                 { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

{
    let next_token lexbuf =
        if not (Queue.is_empty pending_tokens) then
            Queue.take pending_tokens
        else
            token lexbuf
}