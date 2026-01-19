open Ast

exception DivideByZero of string
exception InternalInterpreterError of string

(* the resulting code should only consists of rows and comments *)

(* type var_env = (var, t) Hashtbl.t *) (* TODO: check this is the correct type *)

type func_template = {
    params: var list;
    body: statement list
}
type func_env = (var, func_template) Hashtbl.t (* TODO : change name of func_env *)

let func_defs : func_env = Hashtbl.create 10

let result = ref []


type value =
    | VInt of int
    | VBool of bool
    | VStitch of string
    | VStitchSeqItem of string
    | VStitchSeq of string
    | VRow of string
    | VRowList of string list

let unwrap_int = function
    | VInt(n) -> n
    | _ -> raise (InternalInterpreterError "expected an integer value, found a different type")

let unwrap_bool = function
    | VBool(b) -> b
    | _ -> raise (InternalInterpreterError "expected a boolean value, found a different type")

let unwrap_stitch = function
    | VStitch(st) -> st
    | _ -> raise (InternalInterpreterError "expected a stitch, found a different type")

let unwrap_stitch_seq_item = function
    | VStitchSeqItem(item) -> item
    | _ -> raise (InternalInterpreterError "expected a stitch sequence item, found a different type")

let unwrap_stitch_seq = function
    | VStitchSeq(seq) -> seq
    | _ -> raise (InternalInterpreterError "expected a stitch sequence, found a different type")

let unwrap_row = function
    | VRow(row) -> row
    | _ -> raise (InternalInterpreterError "expected a row, found a different type")

type func_action =
    | Continue
    | Return of t

let eval_function_call env f args =
    let func_data = Hashtbl.find func_defs f in
    let param_list = func_data.params in
    let body = func_data.body in

    let new_env : var_env = Hashtbl.create 10 in
    List.iter2 (fun param arg ->
        let arg_value = eval_argument env arg in
        Hashtbl.add new_env param arg_value
    ) param_list args;

    List.iter (fun stmt ->
        eval_statement new_env stmt
    ) body


let eval_stitch env = function
    | CH -> VStitch("ch")
    | SC -> VStitch("sc")
    | DC -> VStitch("dc")
    | INC -> VStitch("inc")
    | DEC -> VStitch("dec")

(* returns a VInt or a VBool *)
let rec eval_expr env =
    | Int(n) -> VInt(n)
    | Bool(b) -> VBool(b)
    | Var(var) -> Hashtbl.find env var
    | BinOp(e1, op, e2) -> (
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in

        let (v1, v2) = (
            match op with
            | ADD | SUB | MUL | DIV | LT | GT | EQ -> 
                (unwrap_int (eval_expr env e1),
                unwrap_int (eval_expr env e2))
            | AND | OR -> 
                (unwrap_bool (eval_expr env e1),
                unwrap_bool (eval_expr env e2))
        ) in

        match op with
        | ADD -> VInt(v1 + v2)
        | SUB -> VInt(v1 - v2)
        | MUL -> VInt(v1 * v2)
        | DIV -> 
            try
                VInt(v1 / v2)
            with Division_by_zero -> raise (DivideByZero "division by zero encountered in expression evaluation")
        | LT -> VBool(v1 < v2)
        | GT -> VBool(v1 > v2)
        | EQ -> VBool(v1 = v2)
        | AND -> VBool(v1 && v2)
        | OR -> VBool(v1 || v2)
    )
    | UnaryOp(op, e) -> (
        let v = eval_expr env e in
        match op with
        | NEG -> 
            let n = unwrap_int v in
            VInt(-n)
        | NOT -> 
            let b = unwrap_bool v in
            VBool(not b)
    )
    | ExprFuncCall(f, args) -> () (* TODO: implement function evaluation *)
(* returns a string of the mult_expr*)
and eval_mult_expr env = function
    | StitchMultExpr(st, n) ->
        let n_value = unwrap_int (eval_expr env n) in
        let st_value = unwrap_stitch (eval_stitch env st) in
        (Printf.sprintf "%s %d" st_value n_value)
    | StitchSeqMultExpr(seq, n) -> 
        let n_value = unwrap_int (eval_expr env n) in
        let seq_value = unwrap_stitch_seq (eval_stitch_seq env seq) in
        (Printf.sprintf "(%s) x%d" seq_value n_value)
(* returns a VStitchSeqItem string *)
and eval_stitch_seq_item env = function
    | StitchSeqItem(mexpr) -> 
        let v = eval_mult_expr env mexpr in
        VStitchSeqItem(v)
    | StitchSeqItemVar(var) -> Hashtbl.find env var
    | StitchSeqItemFuncCall(f, args) -> () (* TODO: implement function evaluation *)
(* returns a VStitchSeq string *)
and eval_stitch_seq env = function
    | StitchSeq(seq) -> 
        let seq_str = unwrap_stitch_seq_item (String.concat ", " (List.map (eval_stitch_seq_item env) seq)) in
        VStitchSeq(seq_str)
    | StitchSeqVar(var) -> Hashtbl.find env var
    | StitchSeqFuncCall(f, args) -> () (* TODO: implement function evaluation *)
(* returns a VInt, VBool, or VStitchSeq *)
and eval_argument env = function
    | ExprArg(e) -> eval_expr env e
    | StitchSeqArg(seq) -> eval_stitch_seq env seq

(* returns a VRow string *)
let eval_row_lit env = function
    | RowLit(n, seq) ->
        let row_num = unwrap_int (eval_expr env n) in
        let stitch_seq = unwrap_stitch_seq (eval_stitch_seq env seq) in
        let row_str = (Printf.sprintf "R%d: %s\n" row_num stitch_seq) in
        VRow(row_str)

(* returns a VRowList string list *)
let rec eval_row_expr env = function
    | RowVar(var) ->
        let row_list = Hashtbl.find env var in
        let row_list_value = List.map (fun row -> 
            unwrap_row (eval_row_list_item env row)
        ) row_list in
        VRowList(row_list_value)
    | RowFuncCall(f, args) -> () (* TODO: implement function evaluation *)
(* returns a VRow or a VRowList*)
and eval_row_list_item env = function
    | RowLitItem(row) -> eval_row_lit env row
    | RowExpr(row_expr) -> eval_row_expr env row_expr

(* returns the environment because the environment is being changed *)
let eval_definition env = function
    | ExprDef(var, e) -> 
        let v = eval_expr env e in
        Hashtbl.add env var v;
        env
    | StitchSeqDef(var, seq) -> 
        let seq_value = eval_stitch_seq env seq in
        Hashtbl.add env var seq_value;
        env
    | RowListDef(var, row_list) -> 
        let row_list_value = List.map(fun row -> 
            unwrap_row (eval_row_list_item env row)
        ) row_list in
        Hashtbl.add env var VRowList(row_list_value);
        env
    | FuncCallDef(var, f, args) -> () (* TODO: implement function evaluation *)

(* returns a VInt, VBool, VStitchSeq, or VRowList *)
let eval_return_expr env = function
    | ReturnExpr(e) -> eval_expr env e
    | ReturnStitchSeq(seq) -> eval_stitch_seq env seq
    | ReturnRowList(rows) ->
        let row_list_value = List.map (fun row ->
            eval_row_list_item env row
        ) rows in
        VRowList(row_list_value)

(* returns the environment because LetDef will change env *)
let rec eval_statement env = function
    | LetDef(def) ->
        let new_env = eval_definition env def in 
        (None, new_env)
    | Row(row) -> 
        let row = eval_row_lit env row in
        (Some(row), env)
    | RowList(row_expr) -> 
        let row_list = eval_row_expr env row_expr in
        (Some(row_list), env)
    | Return(ret_expr) -> 
        let value = eval_return_expr env ret_expr in
        (Some(value), env)
    | If(cond, then_branch, else_branch) ->
        let cond_value = eval_expr env cond in
        let new_env = (
            if cond_value then
                List.fold_left (fun env_acc stmt ->
                    let (value, new_then_env) = eval_statement env_acc stmt in
                    new_then_env
                ) env then_branch
            else
                List.fold_left (fun env_acc stmt ->
                    let (value, new_else_env) = eval_statement env_acc stmt in
                    new_else_env
                ) env else_branch
        )

let eval_pattern_item env = function
    | FuncDef(f, params, body) ->
        let func_data = { params = params; body = body } in
        Hashtbl.add func_defs f func_data
    | Stmt(stmt) -> let (value, new_env) =eval_statement env stmt in () (* TODO *)

let eval_pattern pattern =
    Hashtbl.clear func_defs;
    let env = Hashtbl.create 10 in
    List.iter (fun item ->
        eval_pattern_item env item
    ) pattern