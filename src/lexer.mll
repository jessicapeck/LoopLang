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


(* TODO: make the scope of the function body be defined by indentation *)
rule token = parse
    | [' ' '\t']                                { token lexbuf }
    | '\n'                                      { next_line lexbuf; NEWLINE }
    | "ch"                                      { CH }
    | "sc"                                      { SC }
    | "inc"                                     { INC }
    | "dec"                                     { DEC }
    | 'r' | 'R' | "row" | "ROW"                 { ROW }
    | 'x' (['0'-'9']+ as num)                   { MULINT (int_of_string num)}
    | "let"                                     { LET }
    | "def"                                     { DEF }
    | "for"                                     { FOR }
    | "to"                                      { TO }
    | ['0'-'9']+ as num                         { INT (int_of_string num) }
    | ['a'-'z' 'A'-'Z' '0'-'9']+ as id          { ID id }
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