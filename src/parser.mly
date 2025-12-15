%{
    open Ast
%}

%token<int> INT MULINT ROWINT
%token<string> ID MULINTVAR ROWINTVAR
%token CH SC DC INC DEC
%token ADD SUB MUL DIV
%token LT GT EQ
%token AND OR NOT
%token LET DEF RETURN FOR TO ASSIGN
%token LPAREN RPAREN COMMA COLON
%token NEWLINE
%token INDENT DEDENT
%token EOF

%start <Ast.pattern> pattern
%%

(* TODO: handle case of empty pattern *)
pattern:
    | pattern_item_list EOF                                                             { Pattern $1 }

pattern_item_list:
    | pattern_item NEWLINE pattern_item_list                                            { $1 :: $3 }
    | pattern_item                                                                      { [$1] }
    | NEWLINE pattern_item_list                                                         { $2 }

pattern_item:
    | DEF ID LPAREN param_list RPAREN COLON NEWLINE INDENT func_statement_list DEDENT   { FuncDef($2, $4, $9) }
    | DEF ID LPAREN RPAREN COLON NEWLINE INDENT func_statement_list DEDENT              { FuncDef($2, [], $8) }
    | statement                                                                         { Stmt($1) }

param_list:
    | ID COMMA param_list                                                               { $1 :: $3 }
    | ID                                                                                { [$1] }

func_statement_list:
    | func_statement NEWLINE func_statement_list                                        { $1 :: $3 }
    | func_statement                                                                    { [$1] }
    | NEWLINE func_statement_list                                                       { $2 }

func_statement:
    | statement                                                                         { $1 }
    | RETURN LPAREN NEWLINE INDENT return_expr DEDENT NEWLINE RPAREN                    { Return($5) }
    | RETURN LPAREN return_expr RPAREN                                                  { Return($3) }

return_expr:
    | int_expr                                                                          { ReturnIntExpr($1) }
    | stitch_list                                                                       { ReturnStitchSeq($1) }
    | statement_expression_list                                                         { ReturnStmtExprList($1) }

statement_expression_list:
    | statement_expression NEWLINE statement_expression_list                            { $1 :: $3 }
    | statement_expression                                                              { [$1] }
    | NEWLINE statement_expression_list                                                 { $2 }

statement:
    | definition                                                                        { LetDef($1) }
    | statement_expression                                                              { StmtExpr($1) }

definition:
    | LET ID ASSIGN stitch_list                                                             { StitchSeqDef($2, $4) }
    | LET ID ASSIGN int_expr                                                                { IntDef($2, $4) }
    | LET ID ASSIGN ID LPAREN arg_list RPAREN                                               { FuncCallDef($2, $4, $6) }

statement_expression:
    | ROWINT COLON stitch_list                                                          { Row(Lit($1), $3) }
    | ROWINTVAR COLON stitch_list                                                       { Row(IntVar($1), $3) }
    | ID LPAREN arg_list RPAREN                                                         { FuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                  { FuncCall($1, []) }

arg_list:
    | arg COMMA arg_list                                                                { $1 :: $3 }
    | arg                                                                               { [$1] }

arg:
    | int_expr                                                                          { NumArg($1) }
    | stitch_list                                                                       { StitchSeqArg($1) }

stitch_list:
    | stitch_seq_item COMMA stitch_list                                                 { $1 :: $3 }
    | stitch_seq_item                                                                   { [$1] }

stitch_seq_item:
    | mult_expr                                                                         { StitchSeqItem($1) }
    | ID                                                                                { StitchSeqItemVar($1) }

mult_expr:
    | CH int_expr                                                                       { StitchMultExpr(CH, $2) }
    | int_expr CH                                                                       { StitchMultExpr(CH, $1) }
    | CH                                                                                { StitchMultExpr(CH, Lit(1)) }
    | SC int_expr                                                                       { StitchMultExpr(SC, $2) }
    | int_expr SC                                                                       { StitchMultExpr(SC, $1) }
    | SC                                                                                { StitchMultExpr(SC, Lit(1)) }
    | DC int_expr                                                                       { StitchMultExpr(DC, $2) }
    | int_expr DC                                                                       { StitchMultExpr(DC, $1) }
    | DC                                                                                { StitchMultExpr(DC, Lit(1)) }
    | INC int_expr                                                                      { StitchMultExpr(INC, $2) }
    | int_expr INC                                                                      { StitchMultExpr(INC, $1) }
    | INC                                                                               { StitchMultExpr(INC, Lit(1)) }
    | DEC int_expr                                                                      { StitchMultExpr(DEC, $2) }
    | int_expr DEC                                                                      { StitchMultExpr(DEC, $1) }
    | DEC                                                                               { StitchMultExpr(DEC, Lit(1)) }
    | LPAREN stitch_list RPAREN MULINT                                                  { StitchSeqMultExpr($2, Lit($4)) }
    | LPAREN stitch_list RPAREN MULINTVAR                                               { StitchSeqMultExpr($2, IntVar($4)) }

int_expr:
    | INT                                                                               { Lit($1) }
    | ID                                                                                { IntVar($1) }

