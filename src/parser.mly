%{
    open Ast
%}

%token<int> INT MULINT
%token<string> ID
%token CH SC INC DEC
%token ROW
%token EQ
%token LET DEF FOR TO
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLON
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

pattern_item:
    (* TODO: make the scope of the function body be defined by indentation *)
    | DEF ID LPAREN param_list RPAREN COLON NEWLINE LBRACE statement_list RBRACE    { FuncDef($2, $4, $9) }
    | statement                                                                     { StmtItem($1) }

param_list:
    | ID COMMA param_list                                                           { $1 :: $3 }
    | ID                                                                            { [$1] }

statement_list:
    | statement NEWLINE statement_list                                              { $1 :: $3 }
    | statement                                                                     { [$1] }

statement:
    | LET ID EQ int_expr                                                            { IntDef($2, $4) }
    | LET ID EQ stitch_list                                                         { StitchListDef($2, $4) }
    | row                                                                           { Row($1) }
    | LET ID EQ ID LPAREN arg_list RPAREN                                           { LetCallDef($2, $4, $6) }

row:
    | ROW int_expr COLON stitch_list                                                { Row($2, $4) }

stitch_list:
    | stitch COMMA stitch_list                                                      { $1 :: $3 }
    | stitch                                                                        { [$1] }

(* TODO: work out how to distinguish between ID, and MULINT with a NumVar *)
stitch:
    | CH MULINT                                                                     { StitchExpr(CH, $2) }
    | CH                                                                            { StitchExpr(CH, 1) }
    | SC MULINT                                                                     { StitchExpr(SC, $2) }
    | SC                                                                            { StitchExpr(SC, 1) }
    | INC MULINT                                                                    { StitchExpr(INC, $2) }
    | INC                                                                           { StitchExpr(INC, 1) }
    | DEC MULINT                                                                    { StitchExpr(DEC, $2) }
    | DEC                                                                           { StitchExpr(DEC, 1) }

int_expr:
    | INT                                                                           { Lit($1) }
    | ID                                                                            { NumVar($1) }

