type var = string

type stitch = CH | SC | INC | DEC

type int_expr = 
  | Lit of int (* Lit(5) *)
  | NumVar of var (* NumVar("i") *)

(* TODO: decide whether users should be allowed to define their own stitch abbreviations *)
type stitch_expr = 
  | StitchExpr of stitch * int_expr (* StitchExpr(SC, Lit(5)) or StitchExpr(SC, NumVar("i")) *)

type stitch_list_item =
  | StitchListItem of stitch_expr
  | StitchListItemVar of var (* StitchExprPartVar("seq") *)

type argument = 
  | NumArg of int_expr
  | StitchListArg of stitch_list_item list

(* TODO: extend function call implementation to allow inlining *)
type statement =
  | IntDef of var * int_expr
  | StitchListDef of var * stitch_list_item list
  | Row of int_expr * stitch_list_item list
  | LetCallDef of var * var * argument list (* var name, func name, args *)

type func_def =
  | FuncDef of var * var list * statement list (* func name, args *)

type pattern_item = 
  | FuncItem of func_def
  | StmtItem of statement

type pattern = 
  | Pattern of pattern_item list

(* string conversions for debugging *)

let string_of_stitch = function
  | CH -> "CH"
  | SC -> "SC"
  | INC -> "INC"
  | DEC -> "DEC"

let string_of_stitch_expr = function
  | Stitch(s, n) -> Printf.sprintf "Stitch(%s, %d)" (string_of_stitch s) n

let string_of_row = function
  | Row(n, expr_list) -> Printf.sprintf "Row(%d, [%s])" n (String.concat ", " (List.map string_of_stitch_expr expr_list))

let string_of_pattern = function
  | Pattern(expr_list) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_row expr_list))
