%{
    open Ast
%}

%token<int> INT MULINT ROWINT
%token<bool> BOOL
%token<string> ID COMMENT
%token ROW MULEXPR
%token CH SC DC INC DEC
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
    | DEF ID LPAREN param_list RPAREN COLON NEWLINE INDENT statement_list DEDENT                                    { FuncDef($2, $4, $9) }
    | DEF ID LPAREN RPAREN COLON NEWLINE INDENT statement_list DEDENT                                               { FuncDef($2, [], $8) }
    | statement                                                                                                     { Stmt($1) }

param_list:
    | ID COMMA param_list                                                                                           { $1 :: $3 }
    | ID                                                                                                            { [$1] }

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
    | FOR ID ASSIGN expr TO expr COLON NEWLINE INDENT statement_list DEDENT                                         { For($2, $4, $6, $10) }

return_expr:
    | expr                                                                                                          { ReturnExpr($1) }
    | stitch_seq                                                                                                    { ReturnStitchSeq($1) }
    | row_list                                                                                                      { ReturnRowList($1) }

definition:
    | LET ID ASSIGN expr                                                                                            { ExprDef($2, $4) }
    | LET ID ASSIGN stitch_seq                                                                                      { StitchSeqDef($2, $4) }
    | LET ID ASSIGN row_list_item                                                                                   { RowListDef($2, [$4]) }
    | LET ID ASSIGN LPAREN NEWLINE INDENT row_list DEDENT NEWLINE RPAREN                                            { RowListDef($2, $7) }
    | LET ID ASSIGN ID LPAREN arg_list RPAREN                                                                       { FuncCallDef($2, $4, $6) }
    | LET ID ASSIGN ID LPAREN RPAREN                                                                                { FuncCallDef($2, $4, []) }

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

row_expr:
    | ID LPAREN arg_list RPAREN                                                                                     { RowFuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                                              { RowFuncCall($1, []) }
    | ID                                                                                                            { RowVar($1) }

arg_list:
    | arg COMMA arg_list                                                                                            { $1 :: $3 }
    | arg                                                                                                           { [$1] }

arg:
    | expr                                                                                                          { ExprArg($1) }
    | LPAREN stitch_seq RPAREN                                                                                      { StitchSeqArg($2) }
    | stitch_seq_expr                                                                                               { StitchSeqArg($1) }
    | stitch_seq_item                                                                                               { StitchSeqArg(StitchSeq([$1])) }

stitch_seq:
    | stitch_seq_item_list                                                                                          { StitchSeq($1) }
    | stitch_seq_expr                                                                                               { $1 }

stitch_seq_expr:
    | ID                                                                                                            { StitchSeqVar($1) }
    | ID LPAREN arg_list RPAREN                                                                                     { StitchSeqFuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                                              { StitchSeqFuncCall($1, []) }

stitch_seq_item_list:
    | stitch_seq_item COMMA stitch_seq_item_list                                                                    { $1 :: $3 }
    | stitch_seq_item                                                                                               { [$1] }

stitch_seq_item:
    | mult_expr comment                                                                                             { StitchSeqItem($1, Some($2)) }
    | mult_expr                                                                                                     { StitchSeqItem($1, None) }
    | ID                                                                                                            { StitchSeqItemVar($1) }
    | ID LPAREN arg_list RPAREN                                                                                     { StitchSeqItemFuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                                              { StitchSeqItemFuncCall($1, []) }

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
    | LPAREN stitch_seq RPAREN MULEXPR expr RPAREN                                                                  { StitchSeqMultExpr($2, $5) }

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
    | ID LPAREN arg_list RPAREN                                                                                     { ExprFuncCall($1, $3) }
    | ID LPAREN RPAREN                                                                                              { ExprFuncCall($1, []) }
    | LPAREN expr RPAREN                                                                                            { $2 }

comment:
    | COMMENT                                                                                                       { Comment($1) }