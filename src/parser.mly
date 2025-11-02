%{
    open Ast
%}

%token<int> INT MULINT
%token<string> ID
%token CH SC INC DEC
%token ROW
%token EQ
%token DEF LET FOR TO
%token LPAREN RPAREN LBRACKET RBRACKET COMMA COLON
%token NEWLINE
%token EOF

%start <Ast.pattern> pattern
%%

pattern:
    | row_list EOF              { Pattern $1 }

row_list:
    | row NEWLINE row_list      { $1 :: $3 }
    | row                       { [$1] }

row:
    | ROW INT COLON stitch_list { Row($2, $4) }

stitch_list:
    | stitch COMMA stitch_list  { $1 :: $3 }
    | stitch                    { [$1] }

stitch:
    | CH MULINT                 { Stitch(CH, $2) }
    | CH                        { Stitch(CH, 1) }
    | SC MULINT                 { Stitch(SC, $2) }
    | SC                        { Stitch(SC, 1) }
    | INC MULINT                { Stitch(INC, $2) }
    | INC                       { Stitch(INC, 1) }
    | DEC MULINT                { Stitch(DEC, $2) }
    | DEC                       { Stitch(DEC, 1) }
