type stitch = CH | SC | INC | DEC

type stitch_expr = 
  | Stitch of stitch * int

type row =
  | Row of int * stitch_expr list

type pattern = 
  | Pattern of row list

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
