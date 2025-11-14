{
    open Parser
    open Lexing

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { 
            pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1 
        }
}

let id_regex = ['a'-'z' 'A'-'Z' '0'-'9']+
let num_regex = ['0'-'9']+

(* TODO: make the scope of the function body be defined by indentation *)
rule token = parse
    | [' ' '\t']                                { token lexbuf }
    | '\n'                                      { next_line lexbuf; NEWLINE }
    | "ch"                                      { CH }
    | "sc"                                      { SC }
    | "dc"                                      { DC }
    | "inc"                                     { INC }
    | "dec"                                     { DEC }
    | 'r' | 'R' | "row" | "ROW"                 { ROW }
    | 'x' (num_regex as num)                    { MULINT (int_of_string num)}
    | "x(" (id_regex as id) ")"                 { MULINTVAR id }
    | "let"                                     { LET }
    | "def"                                     { DEF }
    | "for"                                     { FOR }
    | "to"                                      { TO }
    | num_regex as num                          { INT (int_of_string num) }
    | id_regex as id                            { ID id }
    | '='                                       { EQ }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | '['                                       { LBRACKET }
    | ']'                                       { RBRACKET }
    | '{'                                       { LBRACE }
    | '}'                                       { RBRACE }
    | ':'                                       { COLON }
    | ','                                       { COMMA }
    | eof                                       { EOF }
    | _                                         { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }