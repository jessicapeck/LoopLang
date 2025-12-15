type var = string

type stitch = CH | SC | DC | INC | DEC

type int_bin_op = Add | Sub | Mul | Div
type int_unary_op = Neg

type int_bool_bin_op = LessThan | GreaterThan | EqualTo
type bool_bool_bin_op = And | Or
type bool_unary_op = Not

type int_expr = 
  | Lit of int
  | IntVar of var
  | IntBinOp of int_expr * int_bin_op * int_expr
  | IntUnaryOp of int_unary_op * int_expr

type bool_expr =
  | Bool of bool
  | BoolVar of var
  | IntToBoolBinOp of int_expr * int_bool_bin_op * int_expr
  | BoolToBoolBinOp of bool_expr * bool_bool_bin_op * bool_expr
  | BoolUnaryOp of bool_unary_op * bool_expr

(* TODO: decide whether users should be allowed to define their own stitch abbreviations *)
type mult_expr = 
  | StitchMultExpr of stitch * int_expr
  | StitchSeqMultExpr of stitch_seq_item list * int_expr
and stitch_seq_item =
  | StitchSeqItem of mult_expr
  | StitchSeqItemVar of var

type argument = 
  | NumArg of int_expr
  | StitchSeqArg of stitch_seq_item list

type definition =
  | StitchSeqDef of var * stitch_seq_item list
  | IntDef of var * int_expr
  | FuncCallDef of var * var * argument list (* var name, func name, args *)
  
type stmt_expr =
  | Row of int_expr * stitch_seq_item list (* row number, stitch list*)
  | FuncCall of var * argument list (* func name, args *)
  
type return_expr = 
  | ReturnIntExpr of int_expr
  | ReturnStitchSeq of stitch_seq_item list
  | ReturnStmtExprList of stmt_expr list

(* TODO: extend function call implementation to allow inlining *)
type statement =
  | LetDef of definition
  | StmtExpr of stmt_expr
  | Return of return_expr

type pattern_item = 
  | FuncDef of var * var list * statement list (* func name, args, body *)
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

let string_of_int_expr = function
  | Lit(n) -> Printf.sprintf "Lit(%d)" n
  | IntVar(v) -> Printf.sprintf "IntVar(%s)" v

let rec string_of_mult_expr = function
  | StitchMultExpr(s, n) -> Printf.sprintf "StitchMultExpr(%s, %s)" (string_of_stitch s) (string_of_int_expr n)
  | StitchSeqMultExpr(seq, n) -> Printf.sprintf "StitchSeqMultExpr([%s], %s)" (String.concat ", " (List.map string_of_stitch_seq_item seq)) (string_of_int_expr n)
and string_of_stitch_seq_item = function
  | StitchSeqItem(m) -> string_of_mult_expr m
  | StitchSeqItemVar(v) -> Printf.sprintf "StitchSeqItemVar(%s)" v

let string_of_argument = function
  | NumArg(n) -> Printf.sprintf "NumArg(%s)" (string_of_int_expr n)
  | StitchSeqArg(seq) -> Printf.sprintf "StitchSeqArg([%s])" (String.concat ", " (List.map string_of_stitch_seq_item seq))

let string_of_definition = function
  | IntDef(v, n) -> Printf.sprintf "IntDef(%s, %s)" v (string_of_int_expr n)
  | StitchSeqDef(v, seq) -> Printf.sprintf "StitchSeqDef(%s, [%s])" v (String.concat ", " (List.map string_of_stitch_seq_item seq))
  | FuncCallDef(v, f, args) -> Printf.sprintf "FuncCallDef(%s, %s, [%s])" v f (String.concat ", " (List.map string_of_argument args))

let string_of_stmt_expr = function
  | Row(n, expr_list) -> Printf.sprintf "Row(%s, [%s])" (string_of_int_expr n) (String.concat ", " (List.map string_of_stitch_seq_item expr_list))
  | FuncCall(f, args) -> Printf.sprintf "FuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))

let string_of_return_expr = function
  | ReturnIntExpr(n) -> Printf.sprintf "ReturnIntExpr(%s)" (string_of_int_expr n)
  | ReturnStitchSeq(seq) -> Printf.sprintf "ReturnStitchSeq([%s])" (String.concat ", " (List.map string_of_stitch_seq_item seq))
  | ReturnStmtExprList(e) -> Printf.sprintf "ReturnStmtExprList([%s])" (String.concat ", " (List.map string_of_stmt_expr e))

let string_of_statement = function
  | LetDef(d) -> Printf.sprintf "LetDef(%s)" (string_of_definition d)
  | StmtExpr(e) -> Printf.sprintf "StmtExpr(%s)" (string_of_stmt_expr e)
  | Return(r) -> Printf.sprintf "Return(%s)" (string_of_return_expr r)

let string_of_pattern_item = function
  | FuncDef(f, params, stmts) -> Printf.sprintf "FuncDef(%s, [%s], [%s])" f (String.concat ", " params) (String.concat ", " (List.map string_of_statement stmts))
  | Stmt(s) -> Printf.sprintf "Stmt(%s)" (string_of_statement s)

let string_of_pattern = function
  | Pattern(items) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_pattern_item items))
