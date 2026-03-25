open Ast

exception TypeError of string
exception InternalTypeError of string

type var_env = (var, t) Hashtbl.t
type inference_ctx = (var, t option ref) Hashtbl.t


(* string of type function for error messages *)
let rec string_of_type = function
    | TVar(r) -> (
        match !r with
        | Some(t) -> Printf.sprintf "TVar(Some(%s))" (string_of_type t)
        | None -> "TVar(None)"
    )
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TStitch -> "TStitch"
    | TStitchSeqItem -> "TStitchSeqItem"
    | TStitchSeq -> "TStitchSeq"
    | TRow -> "TRow"
    | TRowList -> "TRowList"
    | TFunc(mangled_f, param_types, return_type) ->
        let param_types_str = String.concat ", " (List.map string_of_type param_types) in
        Printf.sprintf "TFunc(%s, [%s] -> %s)" mangled_f param_types_str (string_of_type return_type)
    | TFuncs(ts) ->
        let func_types = String.concat ", " (List.map string_of_type !ts) in
        Printf.sprintf "TFuncs([%s])" func_types


(* helper functions to unwrap types *)
let rec unwrap_type = function
    | TVar(r) -> (
        match !r with
        | Some(t) -> unwrap_type t
        | None -> TVar(r)
    )
    | t -> t

let unwrap_tfuncs = function
    | TFuncs(ts) -> ts
    | _ -> raise (InternalTypeError "expected TFuncs, found a different type")


(* name mangling function *)
let mangle f param_types return_type =
    let param_types_str = String.concat "_" (List.map string_of_type param_types) in
    let return_type_str = string_of_type return_type in
    Printf.sprintf "%s_%s->%s" f param_types_str return_type_str


(* create new generic type with empty reference *)
let get_new_tvar () =
    TVar(ref None)


(* type inference helper function to safely update context *)
let update_ctx ctx name expected_type =
    match Hashtbl.find_opt ctx name with
    | Some(t_ref) -> (
        match !t_ref with
        | None -> 
            t_ref := Some expected_type;
            expected_type
        | Some(t) ->
            if unwrap_type t <> expected_type then raise (TypeError ("inconsistent type inference for parameter '" ^ name ^ "'"))
            else expected_type
    )
    | None -> raise (TypeError ("undefined variable: '" ^ name ^ "'"))


(* helper function to get the intersection of two environments for conditional branching *)
let rec get_env_intersection env1 env2 =
    let intersection_env : var_env = Hashtbl.create 10 in

    Hashtbl.iter (fun v t1 ->
        try
            let t2 = Hashtbl.find env2 v in
            if t1 = t2 then Hashtbl.add intersection_env v t1
        with Not_found -> ()
    ) env1;
    intersection_env


(* set of functions to check and get function types *)
let copy_function_types param_types return_type =
    let substitutions = ref [] in

    let rec copy_aux = function
        | TVar(r) -> (
            match !r with
            | Some(t) -> copy_aux t
            | None -> 
                if not (List.mem_assoc r !substitutions) then
                    substitutions := (r, get_new_tvar ()) :: !substitutions;

                List.assoc r !substitutions
        )
        | t -> t
    in
    (List.map copy_aux param_types, copy_aux return_type)

let rec check_arg_types env ctx param_types args =
    let correct_arg_types = ref true in
    List.iter2(fun param_type arg ->
        let _, arg_type = check_argument env ctx param_type arg in
        (Printf.printf "arg: %s, param: %s\n" (string_of_type arg_type) (string_of_type param_type));
        match param_type with
        | TVar(t_ref) -> (
            match !t_ref with
            | Some(inner_param_type) -> if arg_type <> inner_param_type then correct_arg_types := false
            | None -> t_ref := Some arg_type
        )
        | t -> (Printf.printf "arg: %s, param: %s\n" (string_of_type arg_type) (string_of_type t)); if arg_type <> t then correct_arg_types := false
    ) param_types args;
    !correct_arg_types

and get_func_types env ctx f args =
    let num_args = List.length args in

    let tfuncs =
        try Hashtbl.find env f
        with Not_found -> raise (TypeError ("undefined function: '" ^ f ^ "'"))
    in
    let func_types = unwrap_tfuncs tfuncs in

    let rec find_correct_type = function
        | [] -> raise (TypeError ("there are no functions named '" ^ f ^ "' that match the number and type of arguments provided"))
        | t :: ts -> (
            let (mangled_f, param_types, return_type) =
                match t with
                | TFunc(m, p, r) -> (m, p, r)
                | _ -> raise (TypeError ("'" ^ f ^"' must be of type TFunc"))
            in
            let num_params = List.length param_types in
            if num_args = num_params then
                let copy_of_param_types, copy_of_return_type = copy_function_types param_types return_type in
                if check_arg_types env ctx copy_of_param_types args then ((Printf.printf "mangled name: %s, params: %s, return type: %s\n" mangled_f (String.concat ", " (List.map string_of_type copy_of_param_types)) (string_of_type copy_of_return_type)); (mangled_f, List.map unwrap_type copy_of_param_types, unwrap_type copy_of_return_type))
                else find_correct_type ts
            else find_correct_type ts
        )
    in
    find_correct_type !func_types


(* main type checking functions *)
and check_stitch env ctx = function
    | CH | SC | DC | INC | DEC | MR | HDC | TR | SLST -> TStitch

and get_var_type env ctx expected_t_opt v = 
    match Hashtbl.find_opt env v with
        | Some(t) -> unwrap_type t (* if v is in env, return its type*)
        | None -> (
            match expected_t_opt with
            | Some(expected_t) -> unwrap_type (update_ctx ctx v expected_t) (* return type inferred from context *)
            | None -> (
                (* check if previously inferred *)
                match Hashtbl.find_opt ctx v with
                | Some(t_ref) -> (
                    match !t_ref with
                    | Some(t) -> unwrap_type t
                    | None ->
                        let generic_type = get_new_tvar () in
                        t_ref := Some generic_type;
                        generic_type
                )
                | None -> raise (TypeError ("undefined variable: '" ^ v ^ "'"))
            )
        )

and check_expr env ctx expected_t_opt = function
    | Int(n) -> (Int(n), TInt)
    | Bool(b) -> (Bool(b), TBool)
    | ExprVar(v) -> 
        let t = get_var_type env ctx expected_t_opt v in
        (ExprVar(v), t)
    | ExprFuncCall(f, args) -> 
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        let args_ast = List.map2 (fun param_type arg ->
            let arg_ast, _ = check_argument env ctx param_type arg in
            arg_ast
        ) param_types args in
        (ExprFuncCall(mangled_f, args_ast), return_type)
    | BinOp(e1, op, e2) -> (
        match op with
        | ADD | SUB | MUL | DIV ->
            let e1_ast, t1 = check_expr env ctx (Some TInt) e1 in
            let e2_ast, t2 = check_expr env ctx (Some TInt) e2 in
            if t1 = TInt && t2 = TInt then (BinOp(e1_ast, op, e2_ast), TInt)
            else raise (TypeError "binary arithmetic operations expect TInt operands")
        | LT | GT | EQ ->
            let e1_ast, t1 = check_expr env ctx (Some TInt) e1 in
            let e2_ast, t2 = check_expr env ctx (Some TInt) e2 in
            if t1 = TInt && t2 = TInt then (BinOp(e1_ast, op, e2_ast), TBool)
            else raise (TypeError "binary comparison operations expect TInt operands")
        | AND | OR ->
            let e1_ast, t1 = check_expr env ctx (Some TBool) e1 in
            let e2_ast, t2 = check_expr env ctx (Some TBool) e2 in
            if t1 = TBool && t2 = TBool then (BinOp(e1_ast, op, e2_ast), TBool)
            else raise (TypeError "binary logical operations expect TBool operands")
    )
    | UnaryOp(op, e) -> (
        match op with
        | NEG ->
            let e_ast, t = check_expr env ctx (Some TInt) e in
            if t = TInt then (UnaryOp(op, e_ast), TInt)
            else raise (TypeError "unary arithmetic operations expect a TInt operand")
        | NOT ->
            let e_ast, t = check_expr env ctx (Some TBool) e in
            if t = TBool then (UnaryOp(op, e_ast), TBool)
            else raise (TypeError "unary logical operations expect a TBool operand")
    )

and check_mult_expr env ctx = function
    | StitchMultExpr(st, e) ->
        let e_ast, t = check_expr env ctx (Some TInt) e in
        if t = TInt then (StitchMultExpr(st, e_ast), TStitchSeqItem)
        else raise (TypeError "stitch multiplier expression expects TInt")
    | StitchSeqMultExpr(seq, e) ->
        let seq_ast, t_seq = check_stitch_seq env ctx TStitchSeq seq in
        if t_seq = TStitchSeq then
            let e_ast, t_e = check_expr env ctx (Some TInt) e in
            if t_e = TInt then (StitchSeqMultExpr(seq_ast, e_ast), TStitchSeqItem)
            else raise (TypeError "stitch sequence multiplier expression expects TInt")
        else raise (TypeError "stitch sequence multiplier expression expects TStitchSeq within parentheses")
    | MirrorExpr(seq1, seq2) ->
        let seq1_ast, t_seq1 = check_stitch_seq env ctx TStitchSeq seq1 in
        let seq2_ast, t_seq2 = check_stitch_seq env ctx TStitchSeq seq2 in
        if (t_seq1 = TStitchSeq && t_seq2 = TStitchSeq) then (MirrorExpr(seq1_ast, seq2_ast), TStitchSeqItem)
        else raise (TypeError "mirror expressions expects two TStitchSeq values")
and check_stitch_seq_item env ctx = function
    | StitchSeqItem(mexpr, c_opt) ->
        let mexpr_ast, t = check_mult_expr env ctx mexpr in
        (StitchSeqItem(mexpr_ast, c_opt), t)
    | StitchSeqItemVar(v) ->
        let t = get_var_type env ctx (Some TStitchSeq) v in
        if t = TStitchSeq then (StitchSeqItemVar(v), TStitchSeqItem)
        else raise (TypeError (Printf.sprintf "variable '%s' expected TStitchSeq, but found %s" v (string_of_type t)))
    | StitchSeqItemFuncCall(f, args) ->
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        if return_type = TStitchSeq then
            let args_ast = List.map2 (fun param_type arg ->
                let arg_ast, _ = check_argument env ctx param_type arg in
                arg_ast
            ) param_types args in
            (StitchSeqItemFuncCall(mangled_f, args_ast), TStitchSeqItem)
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TStitchSeq, but found %s" f (string_of_type return_type)))
and check_stitch_seq env ctx expected_t = function
    | StitchSeq(seq) ->
        let seq_ast = List.map (fun item ->
            let item_ast, t_item = check_stitch_seq_item env ctx item in
            if t_item = TStitchSeqItem then item_ast
            else raise (TypeError "stitch sequence expects TStitchSeqItem values")
        ) seq in
        (StitchSeq(seq_ast), TStitchSeq)
    | StitchSeqVar(v) ->
        let t = get_var_type env ctx (Some TStitchSeq) v in
        if t = TStitchSeq then (StitchSeqVar(v), TStitchSeq)
        else raise (TypeError (Printf.sprintf "variable '%s' expected TStitchSeq, but found %s" v (string_of_type t)))
    | StitchSeqFuncCall(f, args) ->  
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        if return_type = TStitchSeq then 
            let args_ast = List.map2 (fun param_type arg ->
                let arg_ast, _ = check_argument env ctx param_type arg in
                arg_ast
            ) param_types args in
            (StitchSeqFuncCall(mangled_f, args_ast), TStitchSeq)
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TStitchSeq, but found %s" f (string_of_type return_type)))
and check_argument env ctx expected_t = function
    | ArgVar(v) -> 
        let t = get_var_type env ctx (Some expected_t) v in
        (ArgVar(v), t)
    | ArgFuncCall(f, args) ->
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        let args_ast = List.map2 (fun param_type arg ->
            let arg_ast, _ = check_argument env ctx param_type arg in
            arg_ast
        ) param_types args in
        (ArgFuncCall(mangled_f, args_ast), return_type)
    | ArgExpr(e) -> 
        let e_ast, t = check_expr env ctx (Some expected_t) e in
        (ArgExpr(e_ast), t)
    | ArgStitchSeq(seq) -> 
        let seq_ast, t = check_stitch_seq env ctx TStitchSeq seq in
        (ArgStitchSeq(seq_ast), t)
    | ArgRowLit(row) -> 
        let row_ast, t = check_row_lit env ctx row in
        if t = TRow then (ArgRowLit(row_ast), TRowList)
        else raise (TypeError "function argument expects TRow value")

and check_row_lit env ctx = function
    | RowLit(e1, seq, count, c_opt) ->
        let e1_ast, t_e1 = check_expr env ctx (Some TInt) e1 in
        let seq_ast, t_seq = check_stitch_seq env ctx TStitchSeq seq in
        let count_ast, t_count = (
            match count with
            | Some(e) ->
                let e_ast, t_e = check_expr env ctx (Some TInt) e in
                (Some e_ast, t_e)
            | None -> (None, TInt)
        ) in
        if t_e1 = TInt then
            if t_seq = TStitchSeq then
                if t_count = TInt then (RowLit(e1_ast, seq_ast, count_ast, c_opt), TRow)
                else raise (TypeError "row count expects TInt")
            else raise (TypeError "row content expects TStitchSeq")
        else raise (TypeError "row number expects TInt")
    | RowRangeLit((e1, e2), seq, count, c_opt) ->
        let e1_ast, t_e1 = check_expr env ctx (Some TInt) e1 in
        let e2_ast, t_e2 = check_expr env ctx (Some TInt) e2 in
        let seq_ast, t_seq = check_stitch_seq env ctx TStitchSeq seq in
        let count_ast, t_count = (
            match count with
            | Some(e) ->
                let e_ast, t_e = check_expr env ctx (Some TInt) e in
                (Some e_ast, t_e)
            | None -> (None, TInt)
        ) in
        if t_e1 = TInt then
            if t_e2 = TInt then
                if t_seq = TStitchSeq then
                    if t_count = TInt then (RowRangeLit((e1_ast, e2_ast), seq_ast, count_ast, c_opt), TRow)
                    else raise (TypeError "row count expects TInt")
                else raise (TypeError "row content expects TStitchSeq")
            else raise (TypeError "upper bound row number expects TInt")
        else raise (TypeError "lower bound row number expects TInt")

let check_row_expr env ctx = function
    | RowVar(v) ->
        let t = get_var_type env ctx (Some TRowList) v in
        if t = TRowList then (RowVar(v), TRowList)
        else raise (TypeError (Printf.sprintf "variable '%s' expected TRowList, but found %s" v (string_of_type t)))
    | RowFuncCall(f, args) -> 
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        if return_type = TRowList then
            let args_ast = List.map2 (fun param_type arg ->
                let arg_ast, _ = check_argument env ctx param_type arg in
                arg_ast
            ) param_types args in
            (RowFuncCall(mangled_f, args_ast), TRowList)
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TRowList, but found %s" f (string_of_type return_type)))

let check_row_list_item env ctx = function
    | RowLitItem(row) ->
        let row_ast, t = check_row_lit env ctx row in
        (RowLitItem(row_ast), t)
    | RowExpr(row_expr) -> 
        let row_expr_ast, t = check_row_expr env ctx row_expr in
        (RowExpr(row_expr_ast), t)

let check_definition env ctx = function
    | DefVar(v1, v2) -> (
        match Hashtbl.find_opt env v2 with
        | Some(t) -> Hashtbl.add env v1 t; (DefVar(v1, v2), env)
        | None -> (
            match Hashtbl.find_opt ctx v2 with
            | Some(t_ref) -> Hashtbl.add ctx v1 t_ref; (DefVar(v1, v2), env)
            | None -> raise (TypeError ("undefined variable: '" ^ v2 ^ "'"))
        )
    )
    | DefFuncCall(v, (f, args)) ->
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        let args_ast = List.map2 (fun param_type arg ->
            let arg_ast, _ = check_argument env ctx param_type arg in
            arg_ast
        ) param_types args in
        Hashtbl.add env v return_type;
        (DefFuncCall(v, (mangled_f, args_ast)), env)
    | DefExpr(v, e) ->
        let e_ast, t = check_expr env ctx None e in
        Hashtbl.add env v t;
        (DefExpr(v, e_ast), env)
    | DefStitchSeq(v, seq) -> 
        let seq_ast, t = check_stitch_seq env ctx TStitchSeq seq in
        Hashtbl.add env v t;
        (DefStitchSeq(v, seq_ast), env)
    | DefRowList(v, items) ->
        let items_ast = List.map(fun item ->
            let item_ast, t_item = check_row_list_item env ctx item in
            if t_item = TRow then item_ast
            else raise (TypeError "row list definition expects TRow values")
        ) items in
        Hashtbl.add env v TRowList;
        (DefRowList(v, items_ast), env)

let check_return_expr env ctx = function
    | ReturnVar(v) -> 
        let t = get_var_type env ctx None v in
        (ReturnVar(v), t)
    | ReturnFuncCall(f, args) ->
        let mangled_f, param_types, return_type = get_func_types env ctx f args in
        let args_ast = List.map2 (fun param_type arg ->
            let arg_ast, _ = check_argument env ctx param_type arg in
            arg_ast
        ) param_types args in
        (ReturnFuncCall(mangled_f, args_ast), return_type)
    | ReturnExpr(e) ->
        let e_ast, t = check_expr env ctx None e in
        (ReturnExpr(e_ast), t)
    | ReturnStitchSeq(seq) -> 
        let seq_ast, t = check_stitch_seq env ctx TStitchSeq seq in
        (ReturnStitchSeq(seq_ast), t)
    | ReturnRowList(items) ->
        let items_ast = List.map (fun item ->
            let item_ast, t_item = check_row_list_item env ctx item in
            if (t_item = TRow || t_item = TRowList) then item_ast
            else raise (TypeError "row list return expression expects TRow values")
        ) items in
        (ReturnRowList(items_ast), TRowList)

let rec check_statement env ctx = function
    | CommentStmt(c) -> (CommentStmt(c), env, [])
    | LetDef(def) ->
        let def_ast, new_env = check_definition env ctx def in
        (LetDef(def_ast), new_env, [])
    | Row(row) -> 
        let row_ast, _ = check_row_lit env ctx row in
        (Row(row_ast), env, []) 
    | RowList(row_expr) ->
        let row_expr_ast, _ = check_row_expr env ctx row_expr in
        (RowList(row_expr_ast), env, [])
    | Return(ret_expr) ->
        let ret_expr_ast, t = check_return_expr env ctx ret_expr in
        (Return(ret_expr_ast), env, [t])
    | If(cond, then_branch, else_branch) ->
        let cond_ast, t_cond = check_expr env ctx (Some TBool) cond in
        if t_cond = TBool then
            let env_then = Hashtbl.copy env in
            let (then_branch_ast, env_after_then, ret_exprs_in_then) = 
                List.fold_left (fun (stmt_ast_acc, env_acc, ret_exprs_acc) stmt ->
                    let (stmt_ast, new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                    (stmt_ast_acc @ [stmt_ast], new_env, ret_exprs_acc @ new_ret_exprs)
                ) ([], env_then, []) then_branch
            in
            let env_else = Hashtbl.copy env in
            let (else_branch_ast, env_after_else, ret_exprs_in_else) = 
                List.fold_left (fun (stmt_ast_acc, env_acc, ret_exprs_acc) stmt ->
                    let (stmt_ast, new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                    (stmt_ast_acc @ [stmt_ast], new_env, ret_exprs_acc @ new_ret_exprs)
                ) ([], env_else, []) else_branch
            in
            (If(cond_ast, then_branch_ast, else_branch_ast), get_env_intersection env_after_then env_after_else, ret_exprs_in_then @ ret_exprs_in_else)
        else
            raise (TypeError "if-else statement condition expects TBool")
    | For(v, lower, upper, stmts) ->
        let lower_ast, t_lower = check_expr env ctx (Some TInt) lower in
        let upper_ast, t_upper = check_expr env ctx (Some TInt) upper in
        if t_lower = TInt then
            if t_upper = TInt then
                let local_env = Hashtbl.copy env in
                Hashtbl.add local_env v TInt;
                let (stmts_ast, _, ret_exprs_in_for_loop) = 
                    List.fold_left (fun (stmt_ast_acc, env_acc, ret_exprs_acc) stmt ->
                        let (stmt_ast, new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                        (stmt_ast_acc @ [stmt_ast], new_env, ret_exprs_acc @ new_ret_exprs)
                    ) ([], local_env, []) stmts
                in
                (* return original env, local env in for-loop is not carried out into the wider program *)
                (For(v, lower_ast, upper_ast, stmts_ast), env, ret_exprs_in_for_loop)
            else raise (TypeError "upper bound of for-loop expects TInt")
        else raise (TypeError "lower bound of for-loop expects TInt")

let check_pattern_item env = function
    | FuncDef(f, params, body) -> 
        let ctx : inference_ctx = Hashtbl.create 10 in
        List.iter (fun param -> Hashtbl.add ctx param (ref None)) params;

        let rec analyse_body local_env return_types body body_ast_acc =
            match body with
            | [] -> body_ast_acc, return_types
            | stmt :: stmts ->
                let stmt_ast, new_local_env, new_return_types = check_statement local_env ctx stmt
                in analyse_body new_local_env (return_types @ new_return_types) stmts (body_ast_acc @ [stmt_ast])
        in

        (* retain previous function definitions in local env *)
        let local_env : var_env = Hashtbl.create 10 in
        Hashtbl.iter (fun var_name var_type ->
            match var_type with
            | TFuncs(_) -> Hashtbl.add local_env var_name var_type
            | _ -> ()
        ) env;

        (* analyse body to determine return types *)
        let body_ast, all_return_types = analyse_body local_env [] body [] in

        (* check return types are consistent to find overall return type *)
        let return_type =
            match all_return_types with
            | [] -> raise (TypeError ("function '" ^ f ^ "' does not return a value"))
            | t :: ts ->
                List.iter (fun t' ->
                if t' <> t then
                    raise (TypeError ("function '" ^ f ^ "' has inconsistent return types"))
                ) ts;
                t
        in

        (* get parameter types from context *)
        let param_types = List.map(fun param ->
            match Hashtbl.find_opt ctx param with
            | Some(t_ref) -> (
                match !t_ref with
                | Some(t) -> t
                | None ->
                    let generic_type = get_new_tvar () in
                    t_ref := Some generic_type;
                    generic_type
            )
            | None -> raise (TypeError ("unable to infer type of parameter '" ^ param ^ "' in function '" ^ f ^ "'"))
        ) params in

        (* update environment with function type *)
        let mangled_f = mangle f param_types return_type in
        (
            match Hashtbl.find_opt env f with
            | Some(tfuncs) -> 
                let ts = unwrap_tfuncs tfuncs in
                ts := TFunc(mangled_f, param_types, return_type) :: !ts
            | None -> Hashtbl.add env f (TFuncs(ref [TFunc(mangled_f, param_types, return_type)]))
        );
        (FuncDef(mangled_f, params, body_ast), env)
    | Stmt(stmt) -> 
        let dummy_ctx : inference_ctx = Hashtbl.create 0 in
        let stmt_ast, new_env, _ = check_statement env dummy_ctx stmt in
        (Stmt(stmt_ast), new_env)

let check_pattern = function
    | Pattern(items) ->
        let initial_env : var_env = Hashtbl.create 10 in
        let (items_ast, new_env) = (
            List.fold_left(fun (items_ast_acc, env_acc) item ->
                let item_ast, next_env = check_pattern_item env_acc item in
                (items_ast_acc @ [item_ast], next_env)
            ) ([], initial_env) items
        ) in
        (Pattern(items_ast), new_env)
