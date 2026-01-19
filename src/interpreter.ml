open Ast

exception DivideByZero of string

(* the resulting code should only consists of rows and comments *)

type var_env = (var, t) Hashtbl.t (* TODO: check this is the correct type *)

type func_template = {
    params: var list;
    body: statement list
}
type func_env = (var, func_template) Hashtbl.t (* TODO : change name of func_env *)

let result = ref []

let eval_stitch env func_defs = function
    | CH -> "ch"
    | SC -> "sc"
    | DC -> "dc"
    | INC -> "inc"
    | DEC -> "dec"

let rec eval_expr env func_defs =
    | Int(n) -> n
    | Bool(b) -> b
    | Var(var) -> Hashtbl.find env var
    | BinOp(e1, op, e2) ->
        let v1 = eval_expr env func_defs e1 in
        let v2 = eval_expr env func_defs e2 in
        match op with
        | ADD -> v1 + v2
        | SUB -> v1 - v2
        | MUL -> v1 * v2
        | DIV -> 
            try
                v1 / v2
            with Division_by_zero -> raise (DivideByZero "division by zero encountered in expression evaluation")
        | LT -> v1 < v2
        | GT -> v1 > v2
        | EQ -> v1 = v2
        | AND -> v1 && v2
        | OR -> v1 || v2
    | UnaryOp(op, e) ->
        let v = eval_expr env func_defs e in
        match op with
        | NEG -> (-v)
        | NOT -> not v
    | ExprFuncCall(f, args) -> () (* TODO: implement function evaluation *)
and eval_mult_expr env func_defs = function
    | StitchMultExpr(st, n) -> (Printf.sprintf "%s %d" (eval_stitch env func_defs st) (eval_expr env func_defs n))
    | StitchSeqMultExpr(seq, n) -> (Printf.sprintf "(%s) x%d" (eval_stitch_seq env func_defs seq) (eval_expr env func_defs n))
and eval_stitch_seq_item env func_defs = function
    | StitchSeqItem(mexpr) -> eval_mult_expr env func_defs mexpr
    | StitchSeqItemVar(var) -> 
        let v = Hashtbl.find env var in
        eval_stitch_seq_item env func_defs v
    | StitchSeqItemFuncCall(f, args) -> () (* TODO: implement function evaluation *)
and eval_stitch_seq env func_defs = function
    | StitchSeq(seq) ->
        String.concat ", " (List.map (eval_stitch_seq_item env func_defs) seq)
    | StitchSeqVar(var) ->
        let v = Hashtbl.find env var in
        eval_stitch_seq env func_defs v
    | StitchSeqFuncCall(f, args) -> () (* TODO: implement function evaluation *)
and eval_argument env func_defs = function
    | ExprArg(e) -> eval_expr env func_defs e
    | StitchSeqArg(seq) -> eval_stitch_seq env func_defs seq

let eval_row_lit env func_defs = function
    | RowLit(n, seq) ->
        let row_num = eval_expr env func_defs n in
        let stitch_seq = eval_stitch_seq env func_defs seq in
        let row_str = (Printf.sprintf "R%d: %s\n" row_num stitch_seq) in
        (* accumulate rows in results list *)
        result := row_str :: !result

let eval_row_expr env func_defs = function
    | RowVar(var) ->
        let rows = Hashtbl.find env var in
        List.iter (fun row ->
            eval_row_list_item env func_defs row
        ) rows
    | RowFuncCall(f, args) -> () (* TODO: implement function evaluation *)

let eval_row_list_item env func_defs = function
    | RowLitItem(row) -> eval_row_lit env func_defs row
    | RowExpr(row_expr) -> eval_row_expr env func_defs row_expr

(* returns the environment because the environment is being changed *)
let eval_definition env func_defs = function
    | ExprDef(var, e) -> Hashtbl.add env func_defs var e; env
    | StitchSeqDef(var, seq) -> Hashtbl.add env func_defs seq; env
    | RowListDef(var, rows) -> Hashtbl.add env func_defs var rows; env
    | FuncCallDef(var, f, args) -> Hashtbl.add env func_defs var (f, args); env

let eval_return_expr env func_defs = function
    | ReturnExpr(e) -> eval_expr env func_defs e
    | ReturnStitchSeq(seq) -> eval_stitch_seq env func_defs seq
    | ReturnRowList(rows) ->
        List.iter (fun row ->
            eval_row_list_item env func_defs row
        ) rows

(* returns the environment because LetDef will change env *)
let rec eval_statement env func_defs = function
    | LetDef(def) -> eval_definition env func_defs def
    | Row(row) -> 
        let row_str = eval_row_lit env func_defs row in
        env 
    | RowList(row_expr) -> eval_row_expr env func_defs row_expr; env
    | Return(ret_expr) -> eval_return_expr env func_defs ret_expr; env
    | If(cond, then_branch, else_branch) ->
        let cond_value = eval_expr env func_defs cond in
        if cond_value then (* TODO: accumulate func_defs as well as env *)
            List.fold_left (fun env_acc stmt ->
                let new_env = eval_statement env_acc func_defs stmt in
                new_env
            ) env then_branch
        else (* TODO: accumulate func_defs as well as env *)
            List.fold_left (fun env_acc stmt ->
                let new_env = eval_statement env_acc func_defs stmt in
                new_env
            ) env else_branch

let eval_pattern_item env func_defs = function
    | FuncDef(f, params, body) ->
        let func_data = { params = params; body = body } in
        Hashtbl.add func_defs f func_data
    | Stmt(stmt) -> eval_statement env func_defs stmt

let eval_pattern pattern =
    let env : var_env = Hashtbl.create 10 in
    let func_defs : func_env = Hashtbl.create 10 in
    List.iter (fun item ->
        eval_pattern_item env func_defs item
    ) pattern