type t = 
    | TInt 
    | TBool 
    | TStitch
    | TStitchSeqItem
    | TStitchSeq
    | TFunc of t list * t
    | TRow
    | TRowList

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
    | ExprFuncCall of var * argument list (* func name, args *) (* for functions that return: TInt, TBool, TStitch *)
and mult_expr = 
    | StitchMultExpr of stitch * expr (* TStitchSeqItem *)
    | StitchSeqMultExpr of stitch_seq * expr (* TStitchSeqItem *)
and stitch_seq_item =
    | StitchSeqItem of mult_expr
    | StitchSeqItemVar of var
    | StitchSeqItemFuncCall of var * argument list (* func name, args *) (* for functions that return: TStitchSeq, but are used within another stitch seq *)
and stitch_seq =
    | StitchSeq of stitch_seq_item list (* TStitchSeq *)
    | StitchSeqVar of var
    | StitchSeqFuncCall of var * argument list (* func name, args *) (* for functions that return: TStitchSeq *)
and argument = 
    | ExprArg of expr
    | StitchSeqArg of stitch_seq

type row_lit =
    | RowLit of expr * stitch_seq * expr option (* row number, stitch list, row count *) (* TRow *)

type row_expr =
    | RowVar of var (* TRowList *)
    | RowFuncCall of var * argument list (* func name, args *) (* for functions that return: TRowList *)

type row_list_item =
    | RowLitItem of row_lit
    | RowExpr of row_expr

type definition =
    | ExprDef of var * expr
    | StitchSeqDef of var * stitch_seq
    | RowListDef of var * row_list_item list
    | FuncCallDef of var * var * argument list  (* variable name, func name, args *)

type return_expr = 
    | ReturnExpr of expr
    | ReturnStitchSeq of stitch_seq
    | ReturnRowList of row_list_item list

type statement =
    | LetDef of definition
    | Row of row_lit
    | RowList of row_expr
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
    | ExprFuncCall(f, args) -> Printf.sprintf "ExprFuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))
and string_of_mult_expr = function
    | StitchMultExpr(s, n) -> Printf.sprintf "StitchMultExpr(%s, %s)" (string_of_stitch s) (string_of_expr n)
    | StitchSeqMultExpr(seq, n) -> Printf.sprintf "StitchSeqMultExpr(%s, %s)" (string_of_stitch_seq seq) (string_of_expr n)
and string_of_stitch_seq_item = function
    | StitchSeqItem(m) -> Printf.sprintf "StitchSeqItem(%s)" (string_of_mult_expr m)
    | StitchSeqItemVar(v) -> Printf.sprintf "StitchSeqItemVar(%s)" v
    | StitchSeqItemFuncCall(f, args) -> Printf.sprintf "StitchSeqItemFuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))
and string_of_stitch_seq = function
    | StitchSeq(seq) -> Printf.sprintf "StitchSeq([%s])" (String.concat ", " (List.map string_of_stitch_seq_item seq))
    | StitchSeqVar(v) -> Printf.sprintf "StitchSeqVar(%s)" v
    | StitchSeqFuncCall(f, args) -> Printf.sprintf "StitchSeqFuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))
and string_of_argument = function
    | ExprArg(n) -> Printf.sprintf "ExprArg(%s)" (string_of_expr n)
    | StitchSeqArg(seq) -> Printf.sprintf "StitchSeqArg(%s)" (string_of_stitch_seq seq)

let string_of_row_lit = function
    | RowLit(n1, seq, count) -> (
        match count with
        | Some(n2) -> Printf.sprintf "RowLit(%s, %s, Some(%s))" (string_of_expr n1) (string_of_stitch_seq seq) (string_of_expr n2)
        | None -> Printf.sprintf "RowLit(%s, %s, None)" (string_of_expr n1) (string_of_stitch_seq seq)
    )

let string_of_row_expr = function
    | RowVar(v) -> Printf.sprintf "RowVar(%s)" v
    | RowFuncCall(f, args) -> Printf.sprintf "RowFuncCall(%s, [%s])" f (String.concat ", " (List.map string_of_argument args))

let string_of_row_list_item = function
    | RowLitItem(r) -> Printf.sprintf "RowLitItem(%s)" (string_of_row_lit r)
    | RowExpr(e) -> Printf.sprintf "RowExpr(%s)" (string_of_row_expr e)

let string_of_definition = function
    | ExprDef(v, n) -> Printf.sprintf "ExprDef(%s, %s)" v (string_of_expr n)
    | StitchSeqDef(v, seq) -> Printf.sprintf "StitchSeqDef(%s, %s)" v (string_of_stitch_seq seq)
    | RowListDef(v, e) -> Printf.sprintf "RowListDef(%s, [%s])" v (String.concat ", " (List.map string_of_row_list_item e))
    | FuncCallDef(v, f, args) -> Printf.sprintf "FuncCallDef(%s, %s, [%s])" v f (String.concat ", " (List.map string_of_argument args))

let string_of_return_expr = function
    | ReturnExpr(n) -> Printf.sprintf "ReturnExpr(%s)" (string_of_expr n)
    | ReturnStitchSeq(seq) -> Printf.sprintf "ReturnStitchSeq(%s)" (string_of_stitch_seq seq)
    | ReturnRowList(e) -> Printf.sprintf "ReturnRowList([%s])" (String.concat ", " (List.map string_of_row_list_item e))

let rec string_of_statement = function
    | LetDef(d) -> Printf.sprintf "LetDef(%s)" (string_of_definition d)
    | Row(e) -> Printf.sprintf "Row(%s)" (string_of_row_lit e)
    | RowList(e) -> Printf.sprintf "RowList(%s)" (string_of_row_expr e)
    | Return(r) -> Printf.sprintf "Return(%s)" (string_of_return_expr r)
    | If(cond, if_branch, else_branch) -> Printf.sprintf "If(%s, [%s], [%s])" (string_of_expr cond) (String.concat ", " (List.map string_of_statement if_branch)) (String.concat ", " (List.map string_of_statement else_branch))

let string_of_pattern_item = function
    | FuncDef(f, params, stmts) -> Printf.sprintf "FuncDef(%s, [%s], [%s])" f (String.concat ", " params) (String.concat ", " (List.map string_of_statement stmts))
    | Stmt(s) -> Printf.sprintf "Stmt(%s)" (string_of_statement s)

let string_of_pattern = function
    | Pattern(items) -> Printf.sprintf "Pattern([%s])" (String.concat ", " (List.map string_of_pattern_item items))
