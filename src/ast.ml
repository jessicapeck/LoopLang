type stitch = CH | SC | INC | DEC

type expr = 
  | Stitch of int * stitch
  | Row of int * expr list
  | Pattern of expr list
