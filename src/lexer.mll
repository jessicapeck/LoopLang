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


rule token = parse
    | [' ' '\t']                                { token lexbuf }
    | '\n'                                      { next_line lexbuf; NEWLINE }
    | "ch"                                      { CH }
    | "sc"                                      { SC }
    | "inc"                                     { INC }
    | "dec"                                     { DEC }
    | 'r' | 'R' | "row" | "ROW"                 { ROW }
    | 'x'                                       { MUL }
    | "def"                                     { DEF }
    | "let"                                     { LET }
    | "for"                                     { FOR }
    | "to"                                      { TO }
    | ['0'-'9']+ as num                         { INT (int_of_string num) }
    | ['a'-'z' 'A'-'Z']+ as id                  { ID id } (* FIX: if you allow numbers in variable names then this will compete with MUL and INT pairs*)
    | '='                                       { EQ }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | '['                                       { LBRACKET }
    | ']'                                       { RBRACKET }
    | ':'                                       { COLON }
    | ','                                       { COMMA }
    | eof                                       { EOF }
    | _                                         { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }