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
%token EOF

%start pattern
%type <Ast.expr> pattern

pattern:
    | expr EOF { $1}