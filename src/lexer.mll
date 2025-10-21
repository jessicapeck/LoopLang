{
    open Parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']                      { token lexbuf }
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
    | ['0'-'9' 'a'-'z' 'A'-'Z' '_']+ as id      { ID id }
    | '='                                       { EQ }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | '['                                       { LBRACKET }
    | ']'                                       { RBRACKET }
    | ':'                                       { COLON }
    | ','                                       { COMMA }
    | eof                                       { EOF }
    | _                                         { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }