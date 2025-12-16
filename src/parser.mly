%{
    open Ast
%}

%token<int> INT MULINT ROWINT
%token<bool> BOOL
%token<string> ID MULINTVAR ROWINTVAR
%token CH SC DC INC DEC
%token ADD SUB MUL DIV
%token LT GT EQ
%token AND OR NOT
%token IF ELSE LET DEF RETURN FOR TO ASSIGN
%token LPAREN RPAREN COMMA COLON
%token NEWLINE
%token INDENT DEDENT
%token EOF

%start <Ast.pattern> pattern
%%

(* TODO: handle case of empty pattern *)
pattern:
    | pattern_item_list EOF                                                                                         { Pattern $1 }

pattern_item_list:
    | pattern_item NEWLINE pattern_item_list                                                                        { $1 :: $3 }
    | pattern_item                                                                                                  { [$1] }
    | NEWLINE pattern_item_list                                                                                     { $2 }

pattern_item:
    | DEF ID LPAREN param_list RPAREN COLON NEWLINE INDENT statement_list DEDENT                                    { FuncDef($2, $4, $9) }
    | DEF ID LPAREN RPAREN COLON NEWLINE INDENT statement_list DEDENT                                               { FuncDef($2, [], $8) }
    | statement                                                                                                     { Stmt($1) }

param_list:
    | ID COMMA param_list                                                                                           { $1 :: $3 }
    | ID                                                                                                            { [$1] }

statement_list:
    | statement NEWLINE statement_list                                                                              { $1 :: $3 }
    | statement                                                                                                     { [$1] }
    | NEWLINE statement_list                                                                                        { $2 }

statement:
    | definition                                                                                                    { LetDef($1) }
    | statement_expression                                                                                          { StmtExpr($1) }
    | RETURN LPAREN NEWLINE INDENT return_expr DEDENT NEWLINE RPAREN                                                { Return($5) }
    | RETURN LPAREN return_expr RPAREN                                                                              { Return($3) }
    | IF expr COLON NEWLINE INDENT statement_list DEDENT                                                            { If($2, $6, []) }
    | IF expr COLON NEWLINE INDENT statement_list DEDENT NEWLINE ELSE COLON NEWLINE INDENT statement_list DEDENT    { If($2, $6, $13) }

return_expr:
    | expr                                                                                                          { ReturnExpr($1) }
    | stitch_seq                                                                                                    { ReturnStitchSeq($1) }
    | statement_expression_list                                                                                     { ReturnStmtExprList($1) }

definition:
    | LET ID ASSIGN stitch_seq                                                                                      { StitchSeqDef($2, $4) }
    | LET ID ASSIGN expr                                                                                            { ExprDef($2, $4) }
    | LET ID ASSIGN ID LPAREN arg_list RPAREN                                                                       { FuncCallDef($2, $4, $6) }

statement_expression_list:
    | statement_expression NEWLINE statement_expression_list                                                        { $1 :: $3 }
    | statement_expression                                                                                          { [$1] }
    | NEWLINE statement_expression_list                                                                             { $2 }

statement_expression:
    | ROWINT COLON stitch_seq                                                                                       { Row(Int($1), $3) }
    | ROWINTVAR COLON stitch_seq                                                                                    { Row(Var($1), $3) }
    | ID LPAREN arg_list RPAREN                                                                                     { FuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                                              { FuncCall($1, []) }

arg_list:
    | arg COMMA arg_list                                                                                            { $1 :: $3 }
    | arg                                                                                                           { [$1] }

arg:
    | expr                                                                                                          { ExprArg($1) }
    | stitch_seq                                                                                                    { StitchSeqArg($1) }

stitch_seq:
    | stitch_seq_item_list                                                                                          { StitchSeq($1) }
    | ID                                                                                                            { StitchSeqVar($1) }

stitch_seq_item_list:
    | stitch_seq_item COMMA stitch_seq_item_list                                                                    { $1 :: $3 }
    | stitch_seq_item                                                                                               { [$1] }

stitch_seq_item:
    | mult_expr                                                                                                     { StitchSeqItem($1) }
    | ID                                                                                                            { StitchSeqItemVar($1) }

mult_expr:
    | CH expr                                                                                                       { StitchMultExpr(CH, $2) }
    | expr CH                                                                                                       { StitchMultExpr(CH, $1) }
    | CH                                                                                                            { StitchMultExpr(CH, Int(1)) }
    | SC expr                                                                                                       { StitchMultExpr(SC, $2) }
    | expr SC                                                                                                       { StitchMultExpr(SC, $1) }
    | SC                                                                                                            { StitchMultExpr(SC, Int(1)) }
    | DC expr                                                                                                       { StitchMultExpr(DC, $2) }
    | expr DC                                                                                                       { StitchMultExpr(DC, $1) }
    | DC                                                                                                            { StitchMultExpr(DC, Int(1)) }
    | INC expr                                                                                                      { StitchMultExpr(INC, $2) }
    | expr INC                                                                                                      { StitchMultExpr(INC, $1) }
    | INC                                                                                                           { StitchMultExpr(INC, Int(1)) }
    | DEC expr                                                                                                      { StitchMultExpr(DEC, $2) }
    | expr DEC                                                                                                      { StitchMultExpr(DEC, $1) }
    | DEC                                                                                                           { StitchMultExpr(DEC, Int(1)) }
    | LPAREN stitch_seq RPAREN MULINT                                                                               { StitchSeqMultExpr($2, Int($4)) }
    | LPAREN stitch_seq RPAREN MULINTVAR                                                                            { StitchSeqMultExpr($2, Var($4)) }

expr:
    | INT                                                                                                           { Int($1) }
    | BOOL                                                                                                          { Bool($1) }
    | ID                                                                                                            { Var($1) }
    | expr ADD expr                                                                                                 { BinOp($1, ADD, $3) }
    | expr SUB expr                                                                                                 { BinOp($1, SUB, $3) }
    | expr MUL expr                                                                                                 { BinOp($1, MUL, $3) }
    | expr DIV expr                                                                                                 { BinOp($1, DIV, $3) }
    | expr LT expr                                                                                                  { BinOp($1, LT, $3) }
    | expr GT expr                                                                                                  { BinOp($1, GT, $3) }
    | expr EQ expr                                                                                                  { BinOp($1, EQ, $3) }
    | expr AND expr                                                                                                 { BinOp($1, AND, $3) }
    | expr OR expr                                                                                                  { BinOp($1, OR, $3) }
    | SUB expr                                                                                                      { UnaryOp(NEG, $2) }
    | NOT expr                                                                                                      { UnaryOp(NOT, $2) }

