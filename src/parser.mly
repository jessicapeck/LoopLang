%{
    open Ast
%}

%token<int> INT MULINT
%token<string> ID MULINTVAR
%token CH SC DC INC DEC
%token ROW
%token EQ
%token LET DEF FOR TO
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA COLON
%token NEWLINE
%token EOF

%start <Ast.pattern> pattern
%%

(* TODO: handle case of empty pattern *)
pattern:
    | pattern_item_list EOF                                                         { Pattern $1 }

pattern_item_list:
    | pattern_item NEWLINE pattern_item_list                                        { $1 :: $3 }
    | pattern_item                                                                  { [$1] }
    | NEWLINE pattern_item_list                                                     { $2 }

pattern_item:
    (* TODO: make the scope of the function body be defined by indentation *)
    | DEF ID LPAREN param_list RPAREN COLON NEWLINE LBRACE statement_list RBRACE    { FuncDef($2, $4, $9) }
    | statement                                                                     { Stmt($1) }

param_list:
    | ID COMMA param_list                                                           { $1 :: $3 }
    | ID                                                                            { [$1] }

statement_list:
    | statement NEWLINE statement_list                                              { $1 :: $3 }
    | statement                                                                     { [$1] }

statement:
    | LET ID EQ stitch_list                                                         { StitchSeqDef($2, $4) }
    | LET ID EQ int_expr                                                            { IntDef($2, $4) }
    | ROW int_expr COLON stitch_list                                                { Row($2, $4) }
    | LET ID EQ ID LPAREN arg_list RPAREN                                           { LetCallDef($2, $4, $6) }

arg_list:
    | arg COMMA arg_list                                                            { $1 :: $3 }
    | arg                                                                           { [$1] }

arg:
    | int_expr                                                                      { NumArg($1) }
    | stitch_list                                                                   { StitchSeqArg($1) }

stitch_list:
    | stitch_seq_item COMMA stitch_list                                             { $1 :: $3 }
    | stitch_seq_item                                                               { [$1] }

stitch_seq_item:
    | mult_expr                                                                     { StitchSeqItem($1) }
    | ID                                                                            { StitchSeqItemVar($1) }

mult_expr:
    | CH MULINT                                                                     { StitchMultExpr(CH, Lit($2)) }
    | CH MULINTVAR                                                                  { StitchMultExpr(CH, IntVar($2)) }
    | CH                                                                            { StitchMultExpr(CH, Lit(1)) }
    | SC MULINT                                                                     { StitchMultExpr(SC, Lit($2)) }
    | SC MULINTVAR                                                                  { StitchMultExpr(SC, IntVar($2)) }
    | SC                                                                            { StitchMultExpr(SC, Lit(1)) }
    | DC MULINT                                                                     { StitchMultExpr(DC, Lit($2)) }
    | DC MULINTVAR                                                                  { StitchMultExpr(DC, IntVar($2)) }
    | DC                                                                            { StitchMultExpr(DC, Lit(1)) }
    | INC MULINT                                                                    { StitchMultExpr(INC, Lit($2)) }
    | INC MULINTVAR                                                                 { StitchMultExpr(INC, IntVar($2)) }
    | INC                                                                           { StitchMultExpr(INC, Lit(1)) }
    | DEC MULINT                                                                    { StitchMultExpr(DEC, Lit($2)) }
    | DEC MULINTVAR                                                                 { StitchMultExpr(DEC, IntVar($2)) }
    | DEC                                                                           { StitchMultExpr(DEC, Lit(1)) }
    | LBRACKET stitch_list RBRACKET MULINT                                          { StitchSeqMultExpr($2, Lit($4)) }
    | LBRACKET stitch_list RBRACKET MULINTVAR                                       { StitchSeqMultExpr($2, IntVar($4)) }

int_expr:
    | INT                                                                           { Lit($1) }
    | ID                                                                            { IntVar($1) }

