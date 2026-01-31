open Ast

exception DivideByZero of string
exception InternalInterpreterError of string
exception RowNumberError of string

(* the resulting code should only consists of rows and comments *)

(* the type checker will have already validated that the intended value instances are correct *)
type value =
    | VStitch of string
    | VInt of int
    | VBool of bool
    | VStitchMultExpr of string * int
    | VStitchSeqMultExpr of value * int (* value: VStitchSeq *)
    | VStitchSeqItem of value (* value: VStitchMultExpr, VStitchSeqMultExpr *)
    | VStitchSeq of value list (* value: VStitchSeqItem *)
    | VRow of int * value (* value: VStitchSeq *)
    | VRowList of value list (* value: VRow *)
    | Void

type var_env = (var, value) Hashtbl.t

type func_template = {
    params: var list;
    body: statement list
}
type func_env = (var, func_template) Hashtbl.t (* TODO : change name of func_env *)
let func_defs : func_env = Hashtbl.create 10

(* result accumulates the rows for the final crochet pattern *)
let result = ref []

(* next_row_number keeps track of the next expected row number *)
let next_row_number = ref 1


(* UNWRAPPER FUNCTIONS *)

let unwrap_stitch = function
    | VStitch(st) -> st
    | _ -> raise (InternalInterpreterError "expected a stitch value, found a different type")

let unwrap_int = function
    | VInt(n) -> n
    | _ -> raise (InternalInterpreterError "expected an integer value, found a different type")

let unwrap_bool = function
    | VBool(b) -> b
    | _ -> raise (InternalInterpreterError "expected a boolean value, found a different type")

let unwrap_stitch_seq_item = function
    | VStitchSeqItem(item) -> item
    | _ -> raise (InternalInterpreterError "expected a titch sequence item, found a different type")

let unwrap_stitch_seq = function
    | VStitchSeq(seq) -> seq
    | _ -> raise (InternalInterpreterError "expected a stitch sequence, found a different type")

let unwrap_nested_stitch_seqs = function
    | VStitchSeqItem(item) -> [VStitchSeqItem(item)]
    | VStitchSeq(seq) -> seq
    | _ -> raise (InternalInterpreterError "expected a stitch sequence item, found a different type")

let unwrap_row = function
    | VRow(n, seq) -> (n, seq)
    | _ -> raise (InternalInterpreterError "expected a row, found a different type")

let unwrap_row_list = function
        | VRowList(row_list) -> row_list
        | _ -> raise (InternalInterpreterError "expected a row list, found a different type")

let unwrap_nested_rows = function
    | VRow(n, seq) -> [VRow(n, seq)]
    | VRowList(row_list) -> row_list
    | _ -> raise (InternalInterpreterError "expected a row list item, found a different type")


(* ROW NUMBER / ROW COUNT VALIDATION FUNCTIONS *)

let check_row_num row_eval =
    let (actual_row_num, stitch_seq) = unwrap_row row_eval in
    let valid = actual_row_num = !next_row_number in
    (valid, actual_row_num)


(* STRING CONVERSION FUNCTIONS *)

let rec mult_expr_to_str = function
    | VStitchMultExpr(st, n) -> 
        if n = 1 then st
        else (Printf.sprintf "%s %d" st n)
    | VStitchSeqMultExpr(seq, n) -> 
        if n = 1 then stitch_seq_to_str seq
        else (Printf.sprintf "(%s) x%d" (stitch_seq_to_str seq) n)
    | _ -> raise (InternalInterpreterError "expected a multiplier expression, found a different type")
and stitch_seq_to_str seq =
    let seq_value = List.map unwrap_stitch_seq_item (unwrap_stitch_seq seq) in
    String.concat ", " (List.map mult_expr_to_str seq_value)

let row_to_str = function
    | VRow(row_num, stitch_seq) ->
        (Printf.sprintf "R%d: %s" row_num (stitch_seq_to_str stitch_seq))
    | _ -> raise (InternalInterpreterError "expected a row, found a different type")


(* CPS MAPPING FUNCTION *)

let rec map_cps f env l k =
    match l with
    | [] -> k []
    | x::xs -> f env x (fun y -> map_cps f env xs (fun ys -> k (y::ys)))


(* EVALUATION FUNCTIONS *)

(* returns VStitch *)
let eval_stitch = function
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
    map_cps eval_argument env args (fun arg_evals ->
        (* bind arguments to parameters *)
        List.iter2 (fun param arg_eval ->
            Hashtbl.add new_env param arg_eval
        ) param_list arg_evals;

        eval_statement_list new_env body (fun final_env -> k_caller Void) (fun ret_eval -> k_caller ret_eval)
    )


(* returns VInt or VBool *)
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

(* returns VStitchMultExpr or VStitchSeqMultExpr *)
and eval_mult_expr env mult_expr k =
    match mult_expr with
    | StitchMultExpr(st, n) ->
        eval_expr env n (fun n_eval ->
            let n_value = unwrap_int n_eval in
            let st_value = unwrap_stitch (eval_stitch st) in
            k (VStitchMultExpr(st_value, n_value)) 
        )
    | StitchSeqMultExpr(seq, n) -> 
        eval_expr env n (fun n_eval ->
            eval_stitch_seq env seq (fun seq_eval ->
                let n_value = unwrap_int n_eval in
                k (VStitchSeqMultExpr(seq_eval, n_value))
            )
        )

(* returns VStitchSeqItem or VStitchSeq *)
and eval_stitch_seq_item env item k = 
    match item with
    | StitchSeqItem(mexpr) ->
        eval_mult_expr env mexpr (fun v ->
            k (VStitchSeqItem(v))
        )
    | StitchSeqItemVar(var) -> 
        let stitch_seq_eval = Hashtbl.find env var in
        k (stitch_seq_eval)
    | StitchSeqItemFuncCall(f, args) -> 
        eval_function_call env f args (fun func_eval ->
            k (func_eval)
        )

(* returns VStitchSeq *)
and eval_stitch_seq env seq k =
    match seq with
    | StitchSeq(seq) -> 
        map_cps eval_stitch_seq_item env seq (fun seq_items ->
            (* flatten any stitch sequences within stitch sequences *)
            let flattened_seq_items = List.flatten (List.map unwrap_nested_stitch_seqs seq_items) in
            k (VStitchSeq(flattened_seq_items))
        )
    | StitchSeqVar(var) -> k (Hashtbl.find env var)
    | StitchSeqFuncCall(f, args) -> eval_function_call env f args k

(* returns VInt, VBool, or VStitchSeq *)
and eval_argument env arg k =
    match arg with
    | ExprArg(e) -> eval_expr env e k
    | StitchSeqArg(seq) -> eval_stitch_seq env seq k

(* returns VRow *)
and eval_row_lit env row_lit k =
    match row_lit with
    | RowLit(n, seq) ->
        eval_expr env n (fun n_eval ->
            eval_stitch_seq env seq (fun seq_eval ->
                let n_value =  unwrap_int n_eval in
                k (VRow(n_value, seq_eval))
            )
        )

(* returns VRowList *)
and eval_row_expr env row_expr k =
    match row_expr with
    | RowVar(var) -> k (Hashtbl.find env var)
    | RowFuncCall(f, args) -> eval_function_call env f args k

(* returns VRow or VRowList *)
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
            let flattened_row_list_items = List.flatten (List.map unwrap_nested_rows row_list_items) in
            Hashtbl.add env var (VRowList(flattened_row_list_items));
            k env
        )
    | FuncCallDef(var, f, args) ->
        eval_function_call env f args (fun func_eval ->
            Hashtbl.add env var func_eval;
            k env
        )

(* returns VInt, VBool, VStitchSeq, or VRowList *)
and eval_return_expr env ret_expr k =
    match ret_expr with
    | ReturnExpr(e) -> eval_expr env e k
    | ReturnStitchSeq(seq) -> eval_stitch_seq env seq k
    | ReturnRowList(row_list) ->
        map_cps eval_row_list_item env row_list (fun row_list_items ->
            let flattened_row_list_items = List.flatten (List.map unwrap_nested_rows row_list_items) in
            k (VRowList(flattened_row_list_items))
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
            let (row_num_correct, actual_row_num) = check_row_num row_eval in
            if row_num_correct then (
                let row_str = row_to_str row_eval in
                result := row_str :: !result;
                next_row_number := !next_row_number + 1;
                k_next env
            )
            else 
                raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number actual_row_num))
        )
    | RowList(row_expr) ->
        eval_row_expr env row_expr (fun row_list_eval ->
            let row_list_value = unwrap_row_list row_list_eval in
            List.iter (fun row_eval ->
                let (row_num_correct, actual_row_num) = check_row_num row_eval in
                if row_num_correct then (
                    let row_str = row_to_str row_eval in
                    result := row_str :: !result;
                    next_row_number := !next_row_number + 1;
                )
                else
                    raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number actual_row_num))
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
    next_row_number := 1;
    Hashtbl.clear func_defs;
    let env : var_env = Hashtbl.create 10 in

    let rec eval_pattern_item_list env items k = 
            match items with
            | [] -> k env
            | item::rest -> eval_pattern_item env item (fun new_env -> eval_pattern_item_list new_env rest k)
    in

    match pattern with
    | Pattern(pattern_items) -> eval_pattern_item_list env pattern_items (fun final_env -> result := List.rev !result; !result)
