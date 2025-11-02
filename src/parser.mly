%{
    open Ast
%}

%token<int> INT
%token<string> ID
%token CH SC INC DEC
%token ROW
%token MUL EQ
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
    | CH MUL INT                { Stitch(CH, $3) }
    | CH                        { Stitch(CH, 1) }
    | SC MUL INT                { Stitch(SC, $3) }
    | SC                        { Stitch(SC, 1) }
    | INC MUL INT               { Stitch(INC, $3) }
    | INC                       { Stitch(INC, 1) }
    | DEC MUL INT               { Stitch(DEC, $3) }
    | DEC                       { Stitch(DEC, 1) }
