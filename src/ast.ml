type t = 
  | TInt 
  | TBool 
  | TStitch
  | TStitchSeqItem
  | TStitchSeq
  | TFunc of t list * t

type env = (string * t) list

type var = string

type stitch = CH | SC | DC | INC | DEC (* TStitch *)

type bin_op = ADD | SUB | MUL | DIV | LT | GT | EQ | AND | OR
type unary_op = NEG | NOT

type expr = 
  | Int of int (* TInt *)
  | Bool of bool (* TBool *)
  | Var of var
  | BinOp of expr * bin_op * expr
  | UnaryOp of unary_op * expr

(* TODO: decide whether I want to allow stitch to be a variable within StitchMultExpr *)
type mult_expr = 
  | StitchMultExpr of stitch * expr (* TStitchSeqItem *)
  | StitchSeqMultExpr of stitch_seq_item list * expr (* TStitchSeqItem *) (* TODO: do I want this to reference stitch_seq ??? *)
and stitch_seq_item =
  | StitchSeqItem of mult_expr
  | StitchSeqItemVar of var

type stitch_seq =
  | StitchSeq of stitch_seq_item list (* TStitchSeq *)
  | StitchSeqVar of var

type argument = 
  | ExprArg of expr
  | StitchSeqArg of stitch_seq

type definition =
  | ExprDef of var * expr
  | StitchSeqDef of var * stitch_seq
  | FuncCallDef of var * var * argument list (* var name, func name, args *)
  
type stmt_expr =
  | Row of expr * stitch_seq (* row number, stitch list*)
  | FuncCall of var * argument list (* func name, args *)
  
type return_expr = 
  | ReturnExpr of expr
  | ReturnStitchSeq of stitch_seq
  | ReturnStmtExprList of stmt_expr list

(* TODO: extend function call implementation to allow inlining *)
type statement =
  | LetDef of definition
  | StmtExpr of stmt_expr
  | Return of return_expr
  | If of expr * statement list * statement list  (* condition, then branch, else branch *)

type pattern_item = 
  | FuncDef of var * var list * statement list (* func name, params, body *)
  | Stmt of statement

type pattern = 
  | Pattern of pattern_item list


(* string conversions for debugging *)

let string_of_stitch = function
  | CH -> "CH"
  | SC -> "SC"
  | DC -> "DC"
  | INC -> "INC"
  | DEC -> "DEC"

let string_of_bin_op = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | AND -> "AND"
  | OR -> "OR"

let string_of_unary_op = function
  | NEG -> "NEG"
  | NOT -> "NOT"

let rec string_of_expr = function
  | Int(n) -> Printf.sprintf "Int(%d)" n
  | Bool(b) -> Printf.sprintf "Bool(%b)" b
  | Var(v) -> Printf.sprintf "Var(%s)" v
  | BinOp(left, op, right) -> Printf.sprintf "BinOp(%s, %s, %s)" (string_of_expr left) (string_of_bin_op op) (string_of_expr right)
  | UnaryOp(op, e) -> Printf.sprintf "UnaryOp(%s, %s)" (string_of_unary_op op) (string_of_expr e)

let rec string_of_mult_expr = function
  | StitchMultExpr(s, n) -> Printf.sprintf "StitchMultExpr(%s, %s)" (string_of_stitch s) (string_of_expr n)
  | StitchSeqMultExpr(seq, n) -> Printf.sprintf "StitchSeqMultExpr(%s, %s)" (String.concat ", " (List.map string_of_stitch_seq_item seq)) (string_of_expr n)
and string_of_stitch_seq_item = function
  | StitchSeqItem(m) -> string_of_mult_expr m
  | StitchSeqItemVar(v) -> Printf.sprintf "StitchSeqItemVar(%s)" v

let string_of_stitch_seq = function
  | StitchSeq(seq) -> Printf.sprintf "StitchSeq([%s])" (String.concat ", " (List.map string_of_stitch_seq_item seq))
  | StitchSeqVar(v) -> Printf.sprintf "StitchSeqVar(%s)" v

let string_of_argument = function
  | ExprArg(n) -> Printf.sprintf "ExprArg(%s)" (string_of_expr n)
  | StitchSeqArg(seq) -> Printf.sprintf "StitchSeqArg(%s)" (string_of_stitch_seq seq)

let string_of_definition = function
  | ExprDef(v, n) -> Printf.sprintf "ExprDef(%s, %s)" v (string_of_expr n)
  | StitchSeqDef(v, seq) -> Printf.sprintf "StitchSeqDef(%s, %s)" v (string_of_stitch_seq seq)
  | FuncCallDef(v, f, args) -> Printf.sprintf "FuncCallDef(%s, %s, [%s])" v f (String.concat ", " (List.map string_of_argument args))

let string_of_stmt_expr = function
  | Row(n, seq) -> Printf.sprintf "Row(%s, %s)" (string_of_expr n) (string_of_stitch_seq seq)
  | FuncCall(f, args) -> Printf.sprintf "FuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))

let string_of_return_expr = function
  | ReturnExpr(n) -> Printf.sprintf "ReturnExpr(%s)" (string_of_expr n)
  | ReturnStitchSeq(seq) -> Printf.sprintf "ReturnStitchSeq(%s)" (string_of_stitch_seq seq)
  | ReturnStmtExprList(e) -> Printf.sprintf "ReturnStmtExprList([%s])" (String.concat ", " (List.map string_of_stmt_expr e))

let rec string_of_statement = function
  | LetDef(d) -> Printf.sprintf "LetDef(%s)" (string_of_definition d)
  | StmtExpr(e) -> Printf.sprintf "StmtExpr(%s)" (string_of_stmt_expr e)
  | Return(r) -> Printf.sprintf "Return(%s)" (string_of_return_expr r)
  | If(cond, if_branch, else_branch) -> Printf.sprintf "If(%s, [%s], [%s])" (string_of_expr cond) (String.concat ", " (List.map string_of_statement if_branch)) (String.concat ", " (List.map string_of_statement else_branch))

let string_of_pattern_item = function
  | FuncDef(f, params, stmts) -> Printf.sprintf "FuncDef(%s, [%s], [%s])" f (String.concat ", " params) (String.concat ", " (List.map string_of_statement stmts))
  | Stmt(s) -> Printf.sprintf "Stmt(%s)" (string_of_statement s)

let string_of_pattern = function
  | Pattern(items) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_pattern_item items))
