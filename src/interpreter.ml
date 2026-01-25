open Ast

exception DivideByZero of string
exception InternalInterpreterError of string

(* the resulting code should only consists of rows and comments *)

(* type var_env = (var, t) Hashtbl.t *) (* TODO: check this is the correct type *)

type value =
    | VInt of int
    | VBool of bool
    | VStitch of string
    | VStitchSeqItem of string
    | VStitchSeq of string
    | VRow of string
    | VRowList of string list
    | Void

type var_env = (var, value) Hashtbl.t

type func_template = {
    params: var list;
    body: statement list
}
type func_env = (var, func_template) Hashtbl.t (* TODO : change name of func_env *)
let func_defs : func_env = Hashtbl.create 10

let result = ref []


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

let unwrap_row_list = function
    | VRowList(row_list) -> row_list
    | _ -> raise (InternalInterpreterError "expected a row list, found a different type")


let rec map_cps f env l k =
    match l with
    | [] -> k []
    | x::xs -> f env x (fun y -> map_cps f env xs (fun ys -> k (y::ys)))


let eval_stitch env = function
    | CH -> VStitch("ch")
    | SC -> VStitch("sc")
    | DC -> VStitch("dc")
    | INC -> VStitch("inc")
    | DEC -> VStitch("dec")


let rec eval_function_call env f args k_caller =
    let func_data = Hashtbl.find func_defs f in
    let param_list = func_data.params in
    let body = func_data.body in

    let new_env : var_env = Hashtbl.create 10 in
    map_cps eval_argument env args (fun arg_values ->
        (* bind arguments to parameters *)
        List.iter2 (fun param arg_value ->
            Hashtbl.add new_env param arg_value
        ) param_list arg_values;

        eval_statement_list new_env body (fun final_env -> k_caller Void) (fun ret_eval -> k_caller ret_eval)
    )

(* returns a VInt or a VBool *)
and eval_expr env e k =
    match e with
    | Int(n) -> k (VInt(n))
    | Bool(b) -> k (VBool(b))
    | Var(var) -> k (Hashtbl.find env var)
    | BinOp(e1, op, e2) -> (
        eval_expr env e1 (fun e1_eval ->
            eval_expr env e2 (fun e2_eval ->
                match op with
                | ADD | SUB | MUL | DIV | LT | GT | EQ ->
                    let n1 = unwrap_int e1_eval in
                    let n2 = unwrap_int e2_eval in
                    (
                        match op with
                        | ADD -> k (VInt(n1 + n2))
                        | SUB -> k (VInt(n1 - n2))
                        | MUL -> k (VInt(n1 * n2))
                        | DIV -> (
                            try
                                k (VInt(n1 / n2))
                            with Division_by_zero -> raise (DivideByZero "division by zero encountered in expression evaluation")
                        )
                        | LT -> k (VBool(n1 < n2))
                        | GT -> k (VBool(n1 > n2))
                        | EQ -> k (VBool(n1 = n2))
                        | _ -> raise (InternalInterpreterError "unreachable case in the evaluation of binary operations that expect integers")
                    )
                | AND | OR ->
                    let b1 = unwrap_bool e1_eval in
                    let b2 = unwrap_bool e2_eval in
                    (
                        match op with
                        | AND -> k (VBool(b1 && b2))
                        | OR -> k (VBool(b1 || b2))
                        | _ -> raise (InternalInterpreterError "unreachable case in the evaluation of binary operations that expect booleans")
                    )
            )
        )
    )
    | UnaryOp(op, e) -> (
        eval_expr env e (fun v ->
            match op with
            | NEG -> 
                let n = unwrap_int v in
                k (VInt(-n))
            | NOT -> 
                let b = unwrap_bool v in
                k (VBool(not b))
        )
    )
    | ExprFuncCall(f, args) -> eval_function_call env f args k

(* returns a string of the mult_expr*)
and eval_mult_expr env mult_expr k =
    match mult_expr with
    | StitchMultExpr(st, n) ->
        eval_expr env n (fun n_eval ->
            let n_value = unwrap_int n_eval in
            let st_value = unwrap_stitch (eval_stitch env st) in
            k (Printf.sprintf "%s %d" st_value n_value)
        )
    | StitchSeqMultExpr(seq, n) -> 
        eval_expr env n (fun n_eval ->
            eval_stitch_seq env seq (fun seq_eval ->
                let n_value = unwrap_int n_eval in
                let seq_value = unwrap_stitch_seq seq_eval in
                k (Printf.sprintf "(%s) x%d" seq_value n_value)
            )
        )

(* returns a VStitchSeqItem string *)
and eval_stitch_seq_item env item k = 
    match item with
    | StitchSeqItem(mexpr) ->
        eval_mult_expr env mexpr (fun v ->
            k (VStitchSeqItem(v))
        )
    | StitchSeqItemVar(var) -> k (Hashtbl.find env var)
    | StitchSeqItemFuncCall(f, args) -> eval_function_call env f args k

(* returns a VStitchSeq string *)
and eval_stitch_seq env seq k =
    match seq with
    | StitchSeq(seq) -> 
        map_cps eval_stitch_seq_item env seq (fun seq_items ->
            let seq_str = String.concat ", " (List.map unwrap_stitch_seq_item seq_items) in
            k (VStitchSeq(seq_str))
        )
    | StitchSeqVar(var) -> k (Hashtbl.find env var)
    | StitchSeqFuncCall(f, args) -> eval_function_call env f args k

(* returns a VInt, VBool, or VStitchSeq *)
and eval_argument env arg k =
    match arg with
    | ExprArg(e) -> eval_expr env e k
    | StitchSeqArg(seq) -> eval_stitch_seq env seq k

(* returns a VRow string *)
and eval_row_lit env row_lit k =
    match row_lit with
    | RowLit(n, seq) ->
        eval_expr env n (fun n_eval ->
            eval_stitch_seq env seq (fun seq_eval ->
                let row_num = unwrap_int n_eval in
                let stitch_seq = unwrap_stitch_seq seq_eval in
                let row_str = (Printf.sprintf "R%d: %s\n" row_num stitch_seq) in
                k (VRow(row_str))
            )
        )

(* returns a VRowList string list *)
and eval_row_expr env row_expr k =
    match row_expr with
    | RowVar(var) -> k (Hashtbl.find env var)
    | RowFuncCall(f, args) -> eval_function_call env f args k

(* returns a VRow or a VRowList*)
and eval_row_list_item env item k =
    match item with
    | RowLitItem(row) -> eval_row_lit env row k
    | RowExpr(row_expr) -> eval_row_expr env row_expr k

(* returns the environment because the environment is being changed *)
and eval_definition env definition k =
    match definition with
    | ExprDef(var, e) ->
        eval_expr env e (fun e_eval ->
            Hashtbl.add env var e_eval;
            k env
        )
    | StitchSeqDef(var, seq) ->
        eval_stitch_seq env seq (fun seq_eval ->
            Hashtbl.add env var seq_eval;
            k env
        )
    | RowListDef(var, row_list) ->
        map_cps eval_row_list_item env row_list (fun row_list_items ->
            let row_list_value = List.map unwrap_row row_list_items in
            Hashtbl.add env var (VRowList(row_list_value));
            k env
        )
    | FuncCallDef(var, f, args) ->
        eval_function_call env f args (fun func_eval ->
            Hashtbl.add env var func_eval;
            k env
        )

(* returns a VInt, VBool, VStitchSeq, or VRowList *)
and eval_return_expr env ret_expr k =
    match ret_expr with
    | ReturnExpr(e) -> eval_expr env e k
    | ReturnStitchSeq(seq) -> eval_stitch_seq env seq k
    | ReturnRowList(row_list) ->
        map_cps eval_row_list_item env row_list (fun row_list_items ->
            let row_list_value = List.map unwrap_row row_list_items in
            k (VRowList(row_list_value))
        )

(* returns the environment because LetDef will change env *)
and eval_statement env stmt k_next k_ret =
    match stmt with
    | LetDef(def) ->
        eval_definition env def (fun new_env ->
            k_next new_env
        )
    | Row(row) ->
        eval_row_lit env row (fun row_eval ->
            let row_value = unwrap_row row_eval in
            result := row_value :: !result;
            k_next env
        )
    | RowList(row_expr) ->
        eval_row_expr env row_expr (fun row_list_eval ->
            let row_list_value = unwrap_row_list row_list_eval in
            List.iter (fun row ->
                result := row :: !result
            ) row_list_value;
            k_next env
        )
    | Return(ret_expr) ->
        eval_return_expr env ret_expr (fun ret_expr_eval ->
            k_ret ret_expr_eval
        )
    | If(cond, then_branch, else_branch) ->
        eval_expr env cond (fun cond_eval ->
            let cond_value = unwrap_bool cond_eval in
            if cond_value then
                eval_statement_list env then_branch k_next k_ret
            else
                eval_statement_list env else_branch k_next k_ret
        )
and eval_statement_list env stmts k_next k_ret =
    match stmts with
    | [] -> k_next env
    | stmt::rest -> eval_statement env stmt (fun new_env -> eval_statement_list new_env rest k_next k_ret) k_ret

let eval_pattern_item env item k =
    match item with
    | FuncDef(f, params, body) ->
        let func_data = { params = params; body = body } in
        Hashtbl.add func_defs f func_data;
         k env
    | Stmt(stmt) -> eval_statement env stmt (fun new_env -> k new_env) (fun _ -> raise (InternalInterpreterError "return statement not allowed at top level"))

let eval_pattern pattern =
    result := [];
    Hashtbl.clear func_defs;
    let env : var_env = Hashtbl.create 10 in

    let rec eval_pattern_item_list env items k = 
            match items with
            | [] -> k env
            | item::rest -> eval_pattern_item env item (fun new_env -> eval_pattern_item_list new_env rest k)
    in

    match pattern with
    | Pattern(pattern_items) -> eval_pattern_item_list env pattern_items (fun final_env -> result := List.rev !result; !result)