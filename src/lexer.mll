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
    | Indent
    | Dedent
    | Skip

    let process_indentation seq = 
        begin
            (* calculate width of the indent *)
            let current_indent_width = calc_indent_width seq in

            (* add calculated indent width to the stack if the stack is empty*)
            if Stack.is_empty indent_stack then
                begin
                    Stack.push current_indent_width indent_stack;
                    Skip;
                end
            else
                begin
                    let prev_indent_count = Stack.top indent_stack in

                    (* add INDENT token to pending token queue if width increases *)
                    if current_indent_width > prev_indent_count then
                        begin
                            Stack.push current_indent_width indent_stack;
                            Queue.add INDENT pending_tokens;
                            Indent
                        end
                    (* add DEDENT tokens to pending token queue if width decreases*)
                    else if current_indent_width < prev_indent_count then
                        begin
                            while not (Stack.is_empty indent_stack) && (current_indent_width < Stack.top indent_stack) do
                                let _ = Stack.pop indent_stack in
                                Queue.add DEDENT pending_tokens
                            done;
                            Dedent
                        end
                    else
                        Skip
                end
        end
}

let id_regex = ['a'-'z' 'A'-'Z' '0'-'9']+
let num_regex = ['0'-'9']+
let bool_regex = ("true" | "True" | "TRUE" | "false" | "False" | "FALSE")
let row_ident_regex = ('r' | 'R' | "row" | "ROW")
let comment_text = [^ '<' '>']*


rule token = parse
    | '\n'+ ((' '*) | ('\t'*) as seq)                                   { 
                                                                          next_line lexbuf; 
                                                                          match process_indentation seq with
                                                                          | Indent -> NEWLINE
                                                                          | Dedent -> Queue.add NEWLINE pending_tokens; Queue.take pending_tokens
                                                                          | Skip -> NEWLINE
                                                                        }
    | ' '                                                               { token lexbuf }
    | "ch" | "CH"                                                       { CH }
    | "sc" | "SC"                                                       { SC }
    | "dc" | "DC"                                                       { DC }
    | "inc" | "INC"                                                     { INC }
    | "dec" | "DEC"                                                     { DEC }
    | "mr" | "MR"                                                       { MR }
    | "hdc" | "HDC"                                                     { HDC }
    | "tr" | "TR"                                                       { TR }
    | "sl st" | "SL ST" | "slst" | "SLST"                               { SLST }
    | row_ident_regex                                                   { ROW }
    | row_ident_regex ' '? (num_regex as num)                           { ROWINT (int_of_string num) }
    | row_ident_regex ' '? (num_regex as num1) '-' (num_regex as num2)  { ROWINTRANGE ((int_of_string num1, int_of_string num2)) }
    | 'x' (num_regex as num)                                            { MULINT (int_of_string num)}
    | "x("                                                              { MULEXPR }
    | "if"                                                              { IF }
    | "else"                                                            { ELSE }
    | "let"                                                             { LET }
    | "def"                                                             { DEF }
    | "return"                                                          { RETURN } 
    | "for"                                                             { FOR }
    | "to"                                                              { TO }
    | "and"                                                             { AND }
    | "or"                                                              { OR }
    | "not"                                                             { NOT }
    | num_regex as num                                                  { INT (int_of_string num) }
    | bool_regex as b                                                   { BOOL (b = "true" || b = "True" || b = "TRUE") }
    | id_regex as id                                                    { ID id }
    | "<<" (comment_text as txt) ">>"                                   { COMMENT txt }
    | '+'                                                               { ADD }
    | '-'                                                               { SUB }
    | '*'                                                               { MUL }
    | '/'                                                               { DIV }
    | '<'                                                               { LT }
    | '>'                                                               { GT }
    | "=="                                                              { EQ }
    | '='                                                               { ASSIGN }
    | '('                                                               { LPAREN }
    | ')'                                                               { RPAREN }
    | '['                                                               { LBRACKET }
    | ']'                                                               { RBRACKET }
    | ':'                                                               { COLON }
    | ','                                                               { COMMA }
    | eof                                                               {
                                                                            begin
                                                                                while (Stack.length indent_stack > 1) do
                                                                                    let _ = Stack.pop indent_stack in
                                                                                    Queue.add DEDENT pending_tokens
                                                                                done;
                                                                                Queue.add EOF pending_tokens;
                                                                                Queue.take pending_tokens
                                                                            end
                                                                        }
    | _                                                                 { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }

{
    let next_token lexbuf =
        if not (Queue.is_empty pending_tokens) then
            Queue.take pending_tokens
        else
            token lexbuf


    let string_of_token = function
    | INDENT -> "INDENT"
    | DEDENT -> "DEDENT"
    | NEWLINE -> "NEWLINE"
    | CH -> "CH"
    | SC -> "SC"
    | DC -> "DC"
    | INC -> "INC"
    | DEC -> "DEC"
    | MR -> "MR"
    | HDC -> "HDC"
    | TR -> "TR"
    | SLST -> "SLST"
    | ROW -> "ROW"
    | ROWINT num -> Printf.sprintf "ROWINT(%d)" num
    | ROWINTRANGE (num1, num2) -> Printf.sprintf "ROWINTRANGE(%d,%d)" num1 num2
    | MULINT num -> Printf.sprintf "MULINT(%d)" num
    | MULEXPR -> "MULEXPR"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | LET -> "LET"
    | DEF -> "DEF"
    | RETURN -> "RETURN"
    | FOR -> "FOR"
    | TO -> "TO"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | INT num -> Printf.sprintf "INT(%d)" num
    | ID id -> Printf.sprintf "ID(%s)" id
    | BOOL b -> Printf.sprintf "BOOL(%b)" b
    | COMMENT txt -> Printf.sprintf "COMMENT(%s)" txt
    | ADD -> "ADD"
    | SUB -> "SUB"
    | MUL -> "MUL"
    | DIV -> "DIV"
    | LT -> "LT"
    | GT -> "GT"
    | EQ -> "EQ"
    | ASSIGN -> "ASSIGN"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACKET -> "LBRACKET"
    | RBRACKET -> "RBRACKET"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | EOF -> "EOF"
}
