open Ast

exception DivideByZero of string
exception InternalInterpreterError of string
exception RowNumberError of string
exception RowOneError of string
exception RowCountError of string
exception ForLoopError of string

(* the resulting code should only consists of rows and comments *)

(* the type checker will have already validated that the intended value instances are correct *)
type value =
    | VInt of int
    | VBool of bool
    | VStitchMultExpr of stitch * int
    | VStitchSeqMultExpr of value * int (* value: VStitchSeq *)
    | VStitchSeqItem of value * value option (* value 1: VStitchMultExpr, VStitchSeqMultExpr *) (* value 2: VComment *)
    | VStitchSeq of value list (* value: VStitchSeqItem *)
    | VRow of int * value * int option * value option (* value 1: VStitchSeq *) (* value 2: VComment *)
    | VRowRange of (int * int) * value * int option * value option (* value 1: VStitchSeq *) (* value 2: VComment *)
    | VRowList of value list (* value: VRow *)
    | VComment of string
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

(* prev_row_count keeps track of the row count of the previous row *)
let prev_row_count = ref 0


(* UNWRAPPER FUNCTIONS *)

let unwrap_int = function
    | VInt(n) -> n
    | _ -> raise (InternalInterpreterError "expected an integer value, found a different type")

let unwrap_bool = function
    | VBool(b) -> b
    | _ -> raise (InternalInterpreterError "expected a boolean value, found a different type")

let unwrap_stitch_seq_item = function
    | VStitchSeqItem(item, c_opt) -> (item, c_opt)
    | _ -> raise (InternalInterpreterError "expected a stitch sequence item, found a different type")

let unwrap_stitch_seq = function
    | VStitchSeq(seq) -> seq
    | _ -> raise (InternalInterpreterError "expected a stitch sequence, found a different type")

let unwrap_nested_stitch_seqs = function
    | VStitchSeqItem(item, c_opt) -> [VStitchSeqItem(item, c_opt)]
    | VStitchSeq(seq) -> seq
    | _ -> raise (InternalInterpreterError "expected a stitch sequence item, found a different type")

let unwrap_row = function
    | VRow(n, seq, count_opt, c_opt) -> (n, seq, count_opt, c_opt)
    | _ -> raise (InternalInterpreterError "expected a row, found a different type")

let unwrap_row_range = function
    | VRowRange((n1, n2), seq, count_opt, c_opt) -> ((n1, n2), seq, count_opt, c_opt)
    | _ -> raise (InternalInterpreterError "expected a row range, found a different type")

let unwrap_row_list = function
    | VRowList(row_list) -> row_list
    | _ -> raise (InternalInterpreterError "expected a row list, found a different type")

let unwrap_nested_rows = function
    | VRow(n, seq, count_opt, c_opt) -> [VRow(n, seq, count_opt, c_opt)]
    | VRowList(row_list) -> row_list
    | _ -> raise (InternalInterpreterError "expected a row list item, found a different type")

let unwrap_comment = function
    | VComment(txt) -> txt
    | _ -> raise (InternalInterpreterError "expected a comment, found a different type")


(* ROW NUMBER / ROW COUNT VALIDATION FUNCTIONS *)

let check_row_num row_num next =
    let next_correct = (row_num = next) in
    next_correct

let check_row_range_nums lower_row_num upper_row_num next = 
    let next_correct = (lower_row_num = next) in
    let increasing = (upper_row_num > lower_row_num) in
    (next_correct, increasing)


let rec calculate_mult_expr_count row_num = function
    | VStitchMultExpr(st, n) -> (
        if row_num = 1 then (
            match st with
            | CH -> (n, 0)
            | MR -> (n, 0)
            | _ -> raise (RowOneError "R1 of the pattern can only contain chain stitches")
        )
        else (
            match st with
            | INC -> (n, n)
            | DEC -> (-n, n * 2)
            | CH -> (n, 0)
            | _ -> (0, n)
        )
    )
    | VStitchSeqMultExpr(seq, n) ->
        let seq_results = calculate_stitch_seq_count row_num seq in
        let seq_row_count_change, seq_used_stitch_count = seq_results in
        (seq_row_count_change * n, seq_used_stitch_count * n)
    | _ -> raise (InternalInterpreterError "expected a multiplier expression, found a different type")
and calculate_stitch_seq_count row_num seq =
    let seq_value = List.map (fun item -> 
        let (item_value, _) = unwrap_stitch_seq_item item in
        item_value
    ) (unwrap_stitch_seq seq) in
    let results = List.map (calculate_mult_expr_count row_num) seq_value in
    let row_count_changes, used_stitch_counts = List.split results in
    (List.fold_left (+) 0 row_count_changes, List.fold_left (+) 0 used_stitch_counts)

let calculate_row_count row_num stitch_seq prev =
    let change, used_stitch_count = calculate_stitch_seq_count row_num stitch_seq in
    (prev + change, used_stitch_count)


let given_row_count_correct count_opt expected_row_count =
    match count_opt with
    | Some(count_value) -> (count_value = expected_row_count)
    | None -> true


(* STRING CONVERSION FUNCTIONS *)

let string_of_comment = function
    | Comment(txt) -> (Printf.sprintf "<<%s>>" txt)

let string_of_optional_comment = function
    | Some(c) -> (
        let comment_text = unwrap_comment c in
        (Printf.sprintf " <<%s>>" comment_text)
    )
    | None -> ""

let stitch_to_str = function
    | CH -> "ch"
    | SC -> "sc"
    | DC -> "dc"
    | INC -> "inc"
    | DEC -> "dec"
    | MR -> "mr"
    | HDC -> "hdc"
    | TR -> "tr"
    | SLST -> "sl st"

let rec mult_expr_to_str = function
    | VStitchMultExpr(st, n) -> 
        let st_str = stitch_to_str st in
        if n = 1 then st_str
        else (Printf.sprintf "%s %d" st_str n)
    | VStitchSeqMultExpr(seq, n) -> 
        if n = 1 then stitch_seq_to_str seq
        else (Printf.sprintf "(%s) x%d" (stitch_seq_to_str seq) n)
    | _ -> raise (InternalInterpreterError "expected a multiplier expression, found a different type")
and stitch_seq_to_str seq =
    String.concat ", " (List.map (fun item -> 
        let (item_value, comment_opt) = unwrap_stitch_seq_item item in
        (Printf.sprintf "%s%s" (mult_expr_to_str item_value) (string_of_optional_comment comment_opt))
    ) (unwrap_stitch_seq seq))

let row_to_str row_eval row_count =
    match row_eval with
    | VRow(row_num, stitch_seq, _, comment_opt) ->
        (Printf.sprintf "R%d: %s [%d]%s" row_num (stitch_seq_to_str stitch_seq) row_count (string_of_optional_comment comment_opt))
    | VRowRange((lower_row_num, upper_row_num), stitch_seq, _, comment_opt) ->
        (Printf.sprintf "R%d-%d: %s [%d]%s" lower_row_num upper_row_num (stitch_seq_to_str stitch_seq) row_count (string_of_optional_comment comment_opt))
    | _ -> raise (InternalInterpreterError "expected a row or row range, found a different type")


(* CPS MAPPING FUNCTION *)

let rec map_cps f env l k =
    match l with
    | [] -> k []
    | x::xs -> f env x (fun y -> map_cps f env xs (fun ys -> k (y::ys)))


(* EVALUATION FUNCTIONS *)

(* returns optional VComment *)
let eval_optional_comment env c_opt k =
    match c_opt with
    | Some(c) -> (
        match c with
        | Comment(txt) -> k (Some(VComment(txt)))
    )
    | None -> k None

(* returns VInt, VBool, VStitchSeq, or VRowList *)
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

(* returns optional int *)
and eval_optional_count env count_opt k =
    match count_opt with
    | Some(count) -> (
        eval_expr env count (fun count_eval ->
            let count_value = unwrap_int count_eval in
            k (Some(count_value))
        )
    )
    | None -> k None

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
            k (VStitchMultExpr(st, n_value)) 
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
    | StitchSeqItem(mexpr, c_opt) ->
        eval_mult_expr env mexpr (fun v ->
            eval_optional_comment env c_opt (fun c_opt_eval ->
                k (VStitchSeqItem(v, c_opt_eval))
            )
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

(* returns VRow or VRowRange *)
and eval_row_lit env row_lit k =
    match row_lit with
    | RowLit(n1, seq, count_opt, c_opt) ->
        eval_expr env n1 (fun n1_eval ->
            eval_stitch_seq env seq (fun seq_eval ->
                eval_optional_count env count_opt (fun count_eval ->
                    eval_optional_comment env c_opt (fun c_eval ->
                        let n1_value = unwrap_int n1_eval in
                        k (VRow(n1_value, seq_eval, count_eval, c_eval))
                    )
                )
            )
        )
    | RowRangeLit((n1, n2), seq, count_opt, c_opt) ->
        eval_expr env n1 (fun n1_eval ->
            eval_expr env n2 (fun n2_eval ->
                eval_stitch_seq env seq (fun seq_eval ->
                    eval_optional_count env count_opt (fun count_eval ->
                        eval_optional_comment env c_opt (fun c_eval ->
                            let n1_value = unwrap_int n1_eval in
                            let n2_value = unwrap_int n2_eval in
                            k (VRowRange((n1_value, n2_value), seq_eval, count_eval, c_eval))
                        )
                    )
                )
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
    | CommentStmt(c) ->
        let comment_text = string_of_comment c in
        result := comment_text :: !result;
        k_next env
    | LetDef(def) ->
        eval_definition env def (fun new_env ->
            k_next new_env
        )
    | Row(row) -> (
        eval_row_lit env row (fun row_eval ->
            match row_eval with
            | VRow(row_num, stitch_seq, count_opt, _) -> (
                (* validate row number *)
                let row_num_correct = check_row_num row_num !next_row_number in

                if not row_num_correct then 
                    raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number row_num));

                (* validate row count, and compare given row count against true row count *)
                let row_count, used_stitch_count = calculate_row_count row_num stitch_seq !prev_row_count in

                if used_stitch_count <> !prev_row_count then
                    raise (RowCountError (Printf.sprintf "row number %d is built on top of %d stitches which is inconsistent with the previous row count of %d" row_num used_stitch_count !prev_row_count));

                if not (given_row_count_correct count_opt row_count) then 
                    Printf.eprintf "WARNING: the given row count for row number %d was incorrect, this has been corrected in the result\n" row_num;

                (* update result, row count, and row number states *)
                let row_str = row_to_str row_eval row_count in
                result := row_str :: !result;
                prev_row_count := row_count;
                next_row_number := !next_row_number + 1;

                (* return environment *)
                k_next env
            )
            | VRowRange((lower_row_num, upper_row_num), stitch_seq, count_opt, comment_opt) -> (
                (* validate row numbers *)
                let (row_num_correct, increasing) = check_row_range_nums lower_row_num upper_row_num !next_row_number in

                if not row_num_correct then 
                    raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number lower_row_num));
                if not increasing then
                    raise (RowNumberError (Printf.sprintf "lower bound row number (%d) should be strictly less than upper bound row number (%d)" lower_row_num upper_row_num));

                (* validate row count *)
                for row_num = lower_row_num to upper_row_num do
                    let row_count, used_stitch_count = calculate_row_count row_num stitch_seq !prev_row_count in

                    if used_stitch_count <> !prev_row_count then
                        raise (RowCountError (Printf.sprintf "row number %d is built on top of %d stitches which is inconsistent with the previous row count of %d" row_num used_stitch_count !prev_row_count));

                    if not (given_row_count_correct count_opt row_count) then 
                        Printf.eprintf "WARNING: the given row count for row number %d was incorrect, this has been corrected in the result\n" row_num;

                    (* update row count *)
                    prev_row_count := row_count;
                done;

                (* update result and row number states *)
                let row_count = !prev_row_count in
                let row_str = row_to_str row_eval row_count in
                result := row_str :: !result;
                next_row_number := upper_row_num + 1;

                (* return environment *)
                k_next env
            )
            | _ -> raise (InternalInterpreterError "expected a row or row range, found a different type")
        )
    )
    | RowList(row_expr) -> (
        eval_row_expr env row_expr (fun row_list_eval ->
            let row_list_value = unwrap_row_list row_list_eval in
            List.iter (fun row_eval ->
                match row_eval with
                | VRow(row_num, stitch_seq, count_opt, _) -> (
                    (* validate row number *)
                    let row_num_correct = check_row_num row_num !next_row_number in

                    if not row_num_correct then 
                        raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number row_num));

                    (* validate row count, and compare given row count against true row count *)
                    let row_count, used_stitch_count = calculate_row_count row_num stitch_seq !prev_row_count in

                    if used_stitch_count <> !prev_row_count then
                        raise (RowCountError (Printf.sprintf "row number %d is built on top of %d stitches which is inconsistent with the previous row count of %d" row_num used_stitch_count !prev_row_count));

                    if not (given_row_count_correct count_opt row_count) then 
                        Printf.eprintf "WARNING: the given row count for row number %d was incorrect, this has been corrected in the result\n" row_num;

                    (* update result, row count, and row number states *)
                    let row_str = row_to_str row_eval row_count in
                    result := row_str :: !result;
                    prev_row_count := row_count;
                    next_row_number := !next_row_number + 1;
                )
                | VRowRange((lower_row_num, upper_row_num), stitch_seq, count_opt, comment_opt) -> (
                    (* validate row numbers *)
                    let (row_num_correct, increasing) = check_row_range_nums lower_row_num upper_row_num !next_row_number in

                    if not row_num_correct then 
                        raise (RowNumberError (Printf.sprintf "expected row number %d, but found row number %d in its place" !next_row_number lower_row_num));
                    if not increasing then
                        raise (RowNumberError (Printf.sprintf "lower bound row number (%d) should be strictly less than upper bound row number (%d)" lower_row_num upper_row_num));

                    (* validate row count *)
                    for row_num = lower_row_num to upper_row_num do
                        let row_count, used_stitch_count = calculate_row_count row_num stitch_seq !prev_row_count in

                        if used_stitch_count <> !prev_row_count then
                            raise (RowCountError (Printf.sprintf "row number %d is built on top of %d stitches which is inconsistent with the previous row count of %d" row_num used_stitch_count !prev_row_count));

                        if not (given_row_count_correct count_opt row_count) then 
                            Printf.eprintf "WARNING: the given row count for row number %d was incorrect, this has been corrected in the result\n" row_num;

                        (* update row count *)
                        prev_row_count := row_count;
                    done;

                    (* update result and row number states *)
                    let row_count = !prev_row_count in
                    let row_str = row_to_str row_eval row_count in
                    result := row_str :: !result;
                    next_row_number := upper_row_num + 1;
                )
                | _ -> raise (InternalInterpreterError "expected a row or row range, found a different type")
            ) row_list_value;

            (* return environment *)
            k_next env
        )
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
    | For(v, lower, upper, stmts) ->
        eval_expr env lower (fun lower_eval ->
            eval_expr env upper (fun upper_eval ->
                let lower_value = unwrap_int lower_eval in
                let upper_value = unwrap_int upper_eval in

                if lower_value > upper_value then
                    raise (ForLoopError (Printf.sprintf "the for-loop expects the lower bound to be less than or equal to the upper bound, but found a lower bound of %d and an upper bound of %d being used" lower_value upper_value));

                let loop_env = Hashtbl.copy env in

                let rec take_loop_step curr_i curr_env =
                    if curr_i > upper_value then
                        k_next curr_env
                    else (
                        Hashtbl.replace loop_env v (VInt(curr_i));
                        eval_statement_list loop_env stmts (fun new_loop_env -> take_loop_step (curr_i + 1) new_loop_env) k_ret
                    )
                in

                take_loop_step lower_value loop_env
            )
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
    prev_row_count := 0;
    Hashtbl.clear func_defs;
    let env : var_env = Hashtbl.create 10 in

    let rec eval_pattern_item_list env items k = 
            match items with
            | [] -> k env
            | item::rest -> eval_pattern_item env item (fun new_env -> eval_pattern_item_list new_env rest k)
    in

    match pattern with
    | Pattern(pattern_items) -> eval_pattern_item_list env pattern_items (fun final_env -> result := List.rev !result; !result)
