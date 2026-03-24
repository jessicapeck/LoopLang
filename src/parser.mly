%{
    open Ast
%}

%token<int> INT MULINT ROWINT
%token<(int * int)> ROWINTRANGE
%token<bool> BOOL
%token<string> ID COMMENT
%token ROW MULEXPR
%token CH SC DC INC DEC MR HDC TR SLST
%token ADD SUB MUL DIV
%token LT GT EQ
%token AND OR NOT
%token IF ELSE LET DEF RETURN FOR TO ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLON
%token NEWLINE
%token INDENT DEDENT
%token EOF

%start <Ast.pattern> pattern
%%


pattern:
    | pattern_item_list EOF                                                                                         { Pattern $1 }
    | empty_pattern                                                                                                 { Pattern [] }

empty_pattern:
    | NEWLINE empty_pattern                                                                                         { () }
    | EOF                                                                                                           { () }

pattern_item_list:
    | pattern_item NEWLINE pattern_item_list                                                                        { $1 :: $3 }
    | NEWLINE pattern_item_list                                                                                     { $2 }
    | pattern_item                                                                                                  { [$1] }
    | (* empty *)                                                                                                   { [] }

pattern_item:
    | DEF var LPAREN param_list RPAREN COLON NEWLINE INDENT statement_list DEDENT                                   { FuncDef($2, $4, $9) }
    | DEF var LPAREN RPAREN COLON NEWLINE INDENT statement_list DEDENT                                              { FuncDef($2, [], $8) }
    | statement                                                                                                     { Stmt($1) }

param_list:
    | var COMMA param_list                                                                                          { $1 :: $3 }
    | var                                                                                                           { [$1] }

statement_list:
    | statement NEWLINE statement_list                                                                              { $1 :: $3 }
    | NEWLINE statement_list                                                                                        { $2 }
    | statement                                                                                                     { [$1] }
    | (* empty *)                                                                                                   { [] }

statement:
    | comment                                                                                                       { CommentStmt($1) }
    | definition                                                                                                    { LetDef($1) }
    | row_lit                                                                                                       { Row($1) }
    | row_expr                                                                                                      { RowList($1) }
    | RETURN LPAREN NEWLINE INDENT return_expr DEDENT NEWLINE RPAREN                                                { Return($5) }
    | RETURN LPAREN return_expr RPAREN                                                                              { Return($3) }
    | IF expr COLON NEWLINE INDENT statement_list DEDENT                                                            { If($2, $6, []) }
    | IF expr COLON NEWLINE INDENT statement_list DEDENT NEWLINE ELSE COLON NEWLINE INDENT statement_list DEDENT    { If($2, $6, $13) }
    | FOR var ASSIGN expr TO expr COLON NEWLINE INDENT statement_list DEDENT                                        { For($2, $4, $6, $10) }

return_expr:
    | expr                                                                                                          { ReturnExpr($1) }
    | stitch_seq                                                                                                    { ReturnStitchSeq($1) }
    | row_list                                                                                                      { ReturnRowList($1) }

definition:
    | LET var ASSIGN var                                                                                            { DefVar($2, $4) }
    | LET var ASSIGN func_call                                                                                      { DefFuncCall($2, $4) }
    | LET var ASSIGN expr                                                                                           { DefExpr($2, $4) }
    | LET var ASSIGN stitch_seq                                                                                     { DefStitchSeq($2, $4) }
    | LET var ASSIGN row_list_item                                                                                  { DefRowList($2, [$4]) }
    | LET var ASSIGN LPAREN NEWLINE INDENT row_list DEDENT NEWLINE RPAREN                                           { DefRowList($2, $7) }

row_list:
    | row_list_item NEWLINE row_list                                                                                { $1 :: $3 }
    | NEWLINE row_list                                                                                              { $2 }
    | row_list_item                                                                                                 { [$1] }
    | (* empty *)                                                                                                   { [] }

row_list_item:
    | row_lit                                                                                                       { RowLitItem($1) }
    | row_expr                                                                                                      { RowExpr($1) }

row_lit:
    | ROW expr COLON stitch_seq LBRACKET expr RBRACKET comment                                                      { RowLit($2, $4, Some($6), Some($8)) }
    | ROW expr COLON stitch_seq LBRACKET expr RBRACKET                                                              { RowLit($2, $4, Some($6), None) }
    | ROW expr COLON stitch_seq comment                                                                             { RowLit($2, $4, None, Some($5)) }
    | ROW expr COLON stitch_seq                                                                                     { RowLit($2, $4, None, None) }
    | ROWINT COLON stitch_seq LBRACKET expr RBRACKET comment                                                        { RowLit(Int($1), $3, Some($5), Some($7))}
    | ROWINT COLON stitch_seq LBRACKET expr RBRACKET                                                                { RowLit(Int($1), $3, Some($5), None)}
    | ROWINT COLON stitch_seq comment                                                                               { RowLit(Int($1), $3, None, Some($4))}
    | ROWINT COLON stitch_seq                                                                                       { RowLit(Int($1), $3, None, None)}
    | ROWINTRANGE COLON stitch_seq LBRACKET expr RBRACKET comment                                                   { let (lower, upper) = $1 in RowRangeLit((Int(lower), Int(upper)), $3, Some($5), Some($7))}
    | ROWINTRANGE COLON stitch_seq LBRACKET expr RBRACKET                                                           { let (lower, upper) = $1 in RowRangeLit((Int(lower), Int(upper)), $3, Some($5), None)}
    | ROWINTRANGE COLON stitch_seq comment                                                                          { let (lower, upper) = $1 in RowRangeLit((Int(lower), Int(upper)), $3, None, Some($4))}
    | ROWINTRANGE COLON stitch_seq                                                                                  { let (lower, upper) = $1 in RowRangeLit((Int(lower), Int(upper)), $3, None, None) }

row_expr:
    | var                                                                                                           { RowVar($1) }
    | func_call                                                                                                     { RowFuncCall($1) }

arg_list:
    | arg COMMA arg_list                                                                                            { $1 :: $3 }
    | arg                                                                                                           { [$1] }

arg:
    | var                                                                                                           { ArgVar($1) }
    | func_call                                                                                                     { ArgFuncCall($1) }
    | expr                                                                                                          { ArgExpr($1) }
    | LPAREN stitch_seq RPAREN                                                                                      { ArgStitchSeq($2) }
    | stitch_seq_item                                                                                               { ArgStitchSeq(StitchSeq([$1])) }
    | LPAREN row_lit RPAREN                                                                                         { ArgRowLit($2) }

stitch_seq:
    | stitch_seq_item_list                                                                                          { StitchSeq($1) }
    | stitch_seq_expr                                                                                               { $1 }

stitch_seq_expr:
    | var                                                                                                           { StitchSeqVar($1) }
    | func_call                                                                                                     { StitchSeqFuncCall($1) }

stitch_seq_item_list:
    | stitch_seq_item COMMA stitch_seq_item_list                                                                    { $1 :: $3 }
    | stitch_seq_item                                                                                               { [$1] }

stitch_seq_item:
    | mult_expr comment                                                                                             { StitchSeqItem($1, Some($2)) }
    | mult_expr                                                                                                     { StitchSeqItem($1, None) }
    | var                                                                                                           { StitchSeqItemVar($1) }
    | func_call                                                                                                     { StitchSeqItemFuncCall($1) }

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
    | MR expr                                                                                                       { StitchMultExpr(MR, $2) }
    | expr MR                                                                                                       { StitchMultExpr(MR, $1) }
    | MR                                                                                                            { StitchMultExpr(MR, Int(1)) }
    | HDC expr                                                                                                      { StitchMultExpr(HDC, $2) }
    | expr HDC                                                                                                      { StitchMultExpr(HDC, $1) }
    | HDC                                                                                                           { StitchMultExpr(HDC, Int(1)) }
    | TR expr                                                                                                       { StitchMultExpr(TR, $2) }
    | expr TR                                                                                                       { StitchMultExpr(TR, $1) }
    | TR                                                                                                            { StitchMultExpr(TR, Int(1)) }
    | SLST expr                                                                                                     { StitchMultExpr(SLST, $2) }
    | expr SLST                                                                                                     { StitchMultExpr(SLST, $1) }
    | SLST                                                                                                          { StitchMultExpr(SLST, Int(1)) }
    | LPAREN stitch_seq RPAREN MULINT                                                                               { StitchSeqMultExpr($2, Int($4)) }
    | LPAREN stitch_seq RPAREN MULEXPR expr RPAREN                                                                  { StitchSeqMultExpr($2, $5) }

expr:
    | INT                                                                                                           { Int($1) }
    | BOOL                                                                                                          { Bool($1) }
    | var                                                                                                           { ExprVar($1) }
    | func_call                                                                                                     { ExprFuncCall($1) }
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
    | LPAREN expr RPAREN                                                                                            { $2 }

var:
    | ID                                                                                                            { $1 }

func_call:
    | var LPAREN arg_list RPAREN                                                                                    { ($1, $3) }
    | var LPAREN RPAREN                                                                                             { ($1, []) }

comment:
    | COMMENT                                                                                                       { Comment($1) }