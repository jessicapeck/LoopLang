type stitch = CH | SC | INC | DEC

type expr = 
  | Stitch of stitch * int
  | Row of int * expr list
  | Pattern of expr list

let string_of_stitch = function
  | CH -> "CH"
  | SC -> "SC"
  | INC -> "INC"
  | DEC -> "DEC"

let rec string_of_expr = function
  | Stitch(s, n) -> Printf.sprintf "Stitch(%s %d)" (string_of_stitch s) n
  | Row(n, expr_list) -> Printf.sprintf "Row(%d, [%s])" n (String.concat ", " (List.map string_of_expr expr_list))
  | Pattern(expr_list) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_expr expr_list))