%{
    open Ast
%}

%token<int> INT MULINT ROWINT
%token<string> ID MULINTVAR ROWINTVAR
%token CH SC DC INC DEC
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
    | ROWINT COLON stitch_list                                                      { Row(Lit($1), $3) }
    | ROWINTVAR COLON stitch_list                                                   { Row(IntVar($1), $3) }
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
    | CH int_expr                                                                   { StitchMultExpr(CH, $2) }
    | int_expr CH                                                                   { StitchMultExpr(CH, $1) }
    | CH                                                                            { StitchMultExpr(CH, Lit(1)) }
    | SC int_expr                                                                   { StitchMultExpr(SC, $2) }
    | int_expr SC                                                                   { StitchMultExpr(SC, $1) }
    | SC                                                                            { StitchMultExpr(SC, Lit(1)) }
    | DC int_expr                                                                   { StitchMultExpr(DC, $2) }
    | int_expr DC                                                                   { StitchMultExpr(DC, $1) }
    | DC                                                                            { StitchMultExpr(DC, Lit(1)) }
    | INC int_expr                                                                  { StitchMultExpr(INC, $2) }
    | int_expr INC                                                                  { StitchMultExpr(INC, $1) }
    | INC                                                                           { StitchMultExpr(INC, Lit(1)) }
    | DEC int_expr                                                                  { StitchMultExpr(DEC, $2) }
    | int_expr DEC                                                                  { StitchMultExpr(DEC, $1) }
    | DEC                                                                           { StitchMultExpr(DEC, Lit(1)) }
    | LBRACKET stitch_list RBRACKET MULINT                                          { StitchSeqMultExpr($2, Lit($4)) }
    | LBRACKET stitch_list RBRACKET MULINTVAR                                       { StitchSeqMultExpr($2, IntVar($4)) }

int_expr:
    | INT                                                                           { Lit($1) }
    | ID                                                                            { IntVar($1) }

