type var = string

type stitch = CH | SC | DC | INC | DEC

type int_expr = 
  | Lit of int (* Lit(5) *)
  | IntVar of var (* IntVar("i") *)

(* TODO: decide whether users should be allowed to define their own stitch abbreviations *)
type mult_expr = 
  | StitchMultExpr of stitch * int_expr (* StitchMulExpr(SC, Lit(5)) or StitchMulExpr(SC, IntVar("i")) *)
  | StitchSeqMultExpr of stitch_seq_item list * int_expr (* StitchSeqMultExpr([StitchMulExpr(SC, Lit(5)), StitchMulExpr(INC, Lit(2))]), Lit(5) *)
and stitch_seq_item =
  | StitchSeqItem of mult_expr
  | StitchSeqItemVar of var (* StitchSeqItemVar("seq") *)

type argument = 
  | NumArg of int_expr
  | StitchSeqArg of stitch_seq_item list

(* TODO: extend function call implementation to allow inlining *)
type statement =
  | StitchSeqDef of var * stitch_seq_item list
  | IntDef of var * int_expr
  | Row of int_expr * stitch_seq_item list
  | LetCallDef of var * var * argument list (* var name, func name, args *)
  | FuncCall of var * argument list (* func name, args *)

type pattern_item = 
  | FuncDef of var * var list * statement list (* func name, args *)
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

let string_of_statement = function
  | IntDef(v, n) -> Printf.sprintf "IntDef(%s, %s)" v (string_of_int_expr n)
  | StitchSeqDef(v, seq) -> Printf.sprintf "StitchSeqDef(%s, [%s])" v (String.concat ", " (List.map string_of_stitch_seq_item seq))
  | Row(n, expr_list) -> Printf.sprintf "Row(%s, [%s])" (string_of_int_expr n) (String.concat ", " (List.map string_of_stitch_seq_item expr_list))
  | LetCallDef(v, f, args) -> Printf.sprintf "LetCallDef(%s, %s, [%s])" v f (String.concat ", " (List.map string_of_argument args))
  | FuncCall(f, args) -> Printf.sprintf "FuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))

let string_of_pattern_item = function
  | FuncDef(f, params, stmts) -> Printf.sprintf "FuncDef(%s, [%s], [%s])" f (String.concat ", " params) (String.concat ", " (List.map string_of_statement stmts))
  | Stmt(s) -> Printf.sprintf "Stmt(%s)" (string_of_statement s)

let string_of_pattern = function
  | Pattern(items) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_pattern_item items))
