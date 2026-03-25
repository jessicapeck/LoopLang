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
    | TFunc(param_types, return_type) ->
        let param_types_str = String.concat ", " (List.map string_of_type param_types) in
        "TFunc([" ^ param_types_str ^ "] -> " ^ string_of_type return_type ^ ")"
    | TFuncs(ts) ->
        let func_types = String.concat ", " (List.map string_of_type !ts) in
        "TFuncs([" ^ func_types ^ "])"


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
        let arg_type = check_argument env ctx param_type arg in
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
            let (param_types, return_type) =
                match t with
                | TFunc(p, r) -> (p, r)
                | _ -> raise (TypeError ("'" ^ f ^"' must be of type TFunc"))
            in
            let num_params = List.length param_types in
            if num_args = num_params then
                let copy_of_param_types, copy_of_return_type = copy_function_types param_types return_type in
                if check_arg_types env ctx copy_of_param_types args then ((Printf.printf "params: %s, return type: %s\n" (String.concat ", " (List.map string_of_type copy_of_param_types)) (string_of_type copy_of_return_type)); (copy_of_param_types, copy_of_return_type))
                else find_correct_type ts
            else find_correct_type ts
        )
    in
    find_correct_type !func_types

and get_func_return_type env ctx f args =
    let _, return_type = get_func_types env ctx f args in
    unwrap_type return_type


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
    | Int(_) -> TInt
    | Bool(_) -> TBool
    | ExprVar(v) -> get_var_type env ctx expected_t_opt v
    | ExprFuncCall(f, args) -> get_func_return_type env ctx f args
    | BinOp(e1, op, e2) -> (
        match op with
        | ADD | SUB | MUL | DIV ->
            let t1 = check_expr env ctx (Some TInt) e1 in
            let t2 = check_expr env ctx (Some TInt) e2 in
            if t1 = TInt && t2 = TInt then TInt
            else raise (TypeError "binary arithmetic operations expect TInt operands")
        | LT | GT | EQ ->
            let t1 = check_expr env ctx (Some TInt) e1 in
            let t2 = check_expr env ctx (Some TInt) e2 in
            if t1 = TInt && t2 = TInt then TBool
            else raise (TypeError "binary comparison operations expect TInt operands")
        | AND | OR ->
            let t1 = check_expr env ctx (Some TBool) e1 in
            let t2 = check_expr env ctx (Some TBool) e2 in
            if t1 = TBool && t2 = TBool then TBool
            else raise (TypeError "binary logical operations expect TBool operands")
    )
    | UnaryOp(op, e) -> (
        match op with
        | NEG ->
            let t = check_expr env ctx (Some TInt) e in
            if t = TInt then TInt
            else raise (TypeError "unary arithmetic operations expect a TInt operand")
        | NOT ->
            let t = check_expr env ctx (Some TBool) e in
            if t = TBool then TBool
            else raise (TypeError "unary logical operations expect a TBool operand")
    )

and check_mult_expr env ctx = function
    | StitchMultExpr(st, e) ->
        let t = check_expr env ctx (Some TInt) e in
        if t = TInt then TStitchSeqItem
        else raise (TypeError "stitch multiplier expression expects TInt")
    | StitchSeqMultExpr(seq, e) ->
        let t_seq = check_stitch_seq env ctx TStitchSeq seq in
        if t_seq = TStitchSeq then
            let t_e = check_expr env ctx (Some TInt) e in
            if t_e = TInt then TStitchSeqItem
            else raise (TypeError "stitch sequence multiplier expression expects TInt")
        else raise (TypeError "stitch sequence multiplier expression expects TStitchSeq within parentheses")
and check_stitch_seq_item env ctx = function
    | StitchSeqItem(mexpr, c_opt) -> check_mult_expr env ctx mexpr
    | StitchSeqItemVar(v) ->
        let t = get_var_type env ctx (Some TStitchSeq) v in
        if t = TStitchSeq then TStitchSeqItem
        else raise (TypeError (Printf.sprintf "variable '%s' expected TStitchSeq, but found %s" v (string_of_type t)))
    | StitchSeqItemFuncCall(f, args) ->
        let t = get_func_return_type env ctx f args in
        if t = TStitchSeq then TStitchSeqItem 
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TStitchSeq, but found %s" f (string_of_type t)))
and check_stitch_seq env ctx expected_t = function
    | StitchSeq(seq) ->
        List.iter(fun item ->
            let t_item = check_stitch_seq_item env ctx item in
            if t_item <> TStitchSeqItem then
                raise (TypeError "stitch sequence expects TStitchSeqItem values")
        ) seq;
        TStitchSeq
    | StitchSeqVar(v) ->
        let t = get_var_type env ctx (Some TStitchSeq) v in
        if t = TStitchSeq then TStitchSeq
        else raise (TypeError (Printf.sprintf "variable '%s' expected TStitchSeq, but found %s" v (string_of_type t)))
    | StitchSeqFuncCall(f, args) ->  
        let t = get_func_return_type env ctx f args in
        if t = TStitchSeq then TStitchSeq
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TStitchSeq, but found %s" f (string_of_type t)))
and check_argument env ctx expected_t = function
    | ArgVar(v) -> get_var_type env ctx (Some expected_t) v
    | ArgFuncCall(f, args) -> get_func_return_type env ctx f args
    | ArgExpr(e) -> check_expr env ctx (Some expected_t) e
    | ArgStitchSeq(seq) -> check_stitch_seq env ctx TStitchSeq seq
    | ArgRowLit(row) -> 
        let t = check_row_lit env ctx row in
        if t = TRow then TRowList
        else raise (TypeError "function argument expects TRow value")

and check_row_lit env ctx = function
    | RowLit(e1, seq, count, c_opt) ->
        let t_e1 = check_expr env ctx (Some TInt) e1 in
        let t_seq = check_stitch_seq env ctx TStitchSeq seq in
        let t_count = (
            match count with
            | Some(e) -> check_expr env ctx (Some TInt) e
            | None -> TInt
        ) in
        if t_e1 = TInt then
            if t_seq = TStitchSeq then
                if t_count = TInt then TRow
                else raise (TypeError "row count expects TInt")
            else raise (TypeError "row content expects TStitchSeq")
        else raise (TypeError "row number expects TInt")
    | RowRangeLit((e1, e2), seq, count, c_opt) ->
        let t_e1 = check_expr env ctx (Some TInt) e1 in
        let t_e2 = check_expr env ctx (Some TInt) e2 in
        let t_seq = check_stitch_seq env ctx TStitchSeq seq in
        let t_count = (
            match count with
            | Some(e) -> check_expr env ctx (Some TInt) e
            | None -> TInt
        ) in
        if t_e1 = TInt then
            if t_e2 = TInt then
                if t_seq = TStitchSeq then
                    if t_count = TInt then TRow
                    else raise (TypeError "row count expects TInt")
                else raise (TypeError "row content expects TStitchSeq")
            else raise (TypeError "upper bound row number expects TInt")
        else raise (TypeError "lower bound row number expects TInt")

let check_row_expr env ctx = function
    | RowVar(v) ->
        let t = get_var_type env ctx (Some TRowList) v in
        if t = TRowList then TRowList
        else raise (TypeError (Printf.sprintf "variable '%s' expected TRowList, but found %s" v (string_of_type t)))
    | RowFuncCall(f, args) -> 
        let t = get_func_return_type env ctx f args in
        if t = TRowList then TRowList
        else raise (TypeError (Printf.sprintf "function '%s' expected to return TRowList, but found %s" f (string_of_type t)))

let check_row_list_item env ctx = function
    | RowLitItem(row) -> check_row_lit env ctx row
    | RowExpr(row_expr) -> check_row_expr env ctx row_expr

let check_definition env ctx = function
    | DefVar(v1, v2) -> (
        match Hashtbl.find_opt env v2 with
        | Some(t) -> Hashtbl.add env v1 t; env
        | None -> (
            match Hashtbl.find_opt ctx v2 with
            | Some(t_ref) -> Hashtbl.add ctx v1 t_ref; env
            | None -> raise (TypeError ("undefined variable: '" ^ v2 ^ "'"))
        )
    )
    | DefFuncCall(v, (f, args)) ->
        let return_type = get_func_return_type env ctx f args in
        Hashtbl.add env v return_type;
        env
    | DefExpr(v, e) ->
        let t = check_expr env ctx None e in
        Hashtbl.add env v t;
        env
    | DefStitchSeq(v, seq) -> 
        let t = check_stitch_seq env ctx TStitchSeq seq in
        Hashtbl.add env v t;
        env
    | DefRowList(v, items) ->
        List.iter(fun item ->
            let t_item = check_row_list_item env ctx item in
            if t_item <> TRow then
                raise (TypeError "row list definition expects TRow values")
        ) items;
        Hashtbl.add env v TRowList;
        env

let check_return_expr env ctx = function
    | ReturnVar(v) -> get_var_type env ctx None v
    | ReturnFuncCall(f, args) -> get_func_return_type env ctx f args
    | ReturnExpr(e) -> check_expr env ctx None e
    | ReturnStitchSeq(seq) -> check_stitch_seq env ctx TStitchSeq seq
    | ReturnRowList(items) ->
        List.iter(fun item ->
            let t_item = check_row_list_item env ctx item in
            if (t_item <> TRow && t_item <> TRowList) then
                raise (TypeError "row list return expression expects TRow values")
        ) items;
        TRowList

let rec check_statement env ctx = function
    | CommentStmt(c) -> (env, [])
    | LetDef(def) -> (check_definition env ctx def, [])
    | Row(row) -> 
        let _ = check_row_lit env ctx row in
        (env, []) 
    | RowList(row_expr) ->
        let _ = check_row_expr env ctx row_expr in
        (env, [])
    | Return(ret_expr) ->
        let t = check_return_expr env ctx ret_expr in
        (env, [t])
    | If(cond, then_branch, else_branch) ->
        let t_cond = check_expr env ctx (Some TBool) cond in
        if t_cond = TBool then
            let env_then = Hashtbl.copy env in
            let (env_after_then, ret_exprs_in_then) = 
                List.fold_left (fun (env_acc, ret_exprs_acc) stmt ->
                    let (new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                    (new_env, ret_exprs_acc @ new_ret_exprs)
                ) (env_then, []) then_branch
            in
            let env_else = Hashtbl.copy env in
            let (env_after_else, ret_exprs_in_else) = 
                List.fold_left (fun (env_acc, ret_exprs_acc) stmt ->
                    let (new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                    (new_env, ret_exprs_acc @ new_ret_exprs)
                ) (env_else, []) else_branch
            in
            (get_env_intersection env_after_then env_after_else, ret_exprs_in_then @ ret_exprs_in_else)
        else
            raise (TypeError "if-else statement condition expects TBool")
    | For(v, lower, upper, stmts) ->
        let t_lower = check_expr env ctx (Some TInt) lower in
        let t_upper = check_expr env ctx (Some TInt) upper in
        if t_lower = TInt then
            if t_upper = TInt then
                let local_env = Hashtbl.copy env in
                Hashtbl.add local_env v TInt;
                let (_, ret_exprs_in_for_loop) = 
                    List.fold_left (fun (env_acc, ret_exprs_acc) stmt ->
                        let (new_env, new_ret_exprs) = check_statement env_acc ctx stmt in
                        (new_env, ret_exprs_acc @ new_ret_exprs)
                    ) (local_env, []) stmts
                in
                (* return original env, local env in for-loop is not carried out into the wider program *)
                (env, ret_exprs_in_for_loop)
            else raise (TypeError "upper bound of for-loop expects TInt")
        else raise (TypeError "lower bound of for-loop expects TInt")

let check_pattern_item env = function
    | FuncDef(f, params, body) -> 
        let ctx : inference_ctx = Hashtbl.create 10 in
        List.iter (fun param -> Hashtbl.add ctx param (ref None)) params;

        let rec analyse_body local_env return_types body =
            match body with
            | [] -> return_types
            | stmt :: stmts ->
                let new_local_env, new_return_types = check_statement local_env ctx stmt
                in analyse_body new_local_env (return_types @ new_return_types) stmts
        in

        (* retain previous function definitions in local env *)
        let local_env : var_env = Hashtbl.create 10 in
        Hashtbl.iter (fun var_name var_type ->
            match var_type with
            | TFuncs(_) -> Hashtbl.add local_env var_name var_type
            | _ -> ()
        ) env;

        (* analyse body to determine return types *)
        let all_return_types = analyse_body local_env [] body in

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
        (
            match Hashtbl.find_opt env f with
            | Some(tfuncs) -> 
                let ts = unwrap_tfuncs tfuncs in
                ts := TFunc(param_types, return_type) :: !ts
            | None -> Hashtbl.add env f (TFuncs(ref [TFunc(param_types, return_type)]))
        );
        env
    | Stmt(stmt) -> 
        let dummy_ctx : inference_ctx = Hashtbl.create 0 in
        let new_env, _ = check_statement env dummy_ctx stmt in
        new_env

let check_pattern = function
    | Pattern(items) ->
        let initial_env : var_env = Hashtbl.create 10 in
        List.fold_left(fun env_acc item ->
            check_pattern_item env_acc item
        ) initial_env items
