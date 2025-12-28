exception TypeError of string
exception ArgError of string

(* string of type function for error messages *)
let rec string_of_type = function
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TStitch -> "TStitch"
    | TStitchSeqItem -> "TStitchSeqItem"
    | TStitchSeq -> "TStitchSeq"
    | TStmtExpr -> "TStmtExpr"
    | TStmtExprList -> "TStmtExprList"
    | TRow -> "TRow"
    | TRowList -> "TRowList"
    | TFunc(param_types, return_type) ->
        let param_types_str = String.concat ", " (List.map string_of_type param_types) in
        "TFunc([" ^ param_types_str ^ "] -> " ^ string_of_type return_type ^ ")"

(* set of functions to check and get function types *)
let get_func_types env f =
    let func_type =
        try List.assoc f env
        with Not_found -> raise(TypeError ("Undefined function: " ^ f))
    in
    let (param_types, return_type) = 
        match func_type with
        | TFunc(p, r) -> (p, r)
        | _ -> raise(TypeError "Type error: '" ^ f ^"' is of type TFunc")
    in
    (param_types, return_type)

let check_arg_types env param_types args =
    try
        List.iter2(fun param_type arg ->
            let t = check_argument env arg in
            if t <> param_type then false
        ) param_types args;
        true
    with Invalid_argument -> raise(ArgError "ArgError: number of arguments passed and number of parameters expected do not match")

let get_func_return_type env f args =
    let param_types, return_type = get_func_types env f in
        if check_arg_types env param_types args then
            return_type
        else
            raise(TypeError "Type error: argument types do not match expected parameter types")


(* helper function to get the intersection of two environments for conditional branching *)
let rec get_env_intersection env1 env2 =
    List.filter (fun (v, t1) ->
        try
            let t2 = List.assoc v env2 in
            t1 = t2
        with Not_found -> false
    ) env1


(* main type checking functions *)
let rec check_stitch env = function
    | CH | SC | DC | INC | DEC -> TStitch

let rec check_expr env = function
    | IntLit(_) -> TInt
    | BoolLit(_) -> TBool
    | Var(v) -> (
        try List.assoc v env
        with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
      )
    | BinOp(e1, op, e2) ->
        let t1 = check_expr env e1 in
        let t2 = check_expr env e2 in
        (
            match op with
            | ADD | SUB | MUL | DIV ->
                if t1 = TInt && t2 = TInt then TInt
                else raise(TypeError "Type error: binary arithmetic operations expect TInt operands")
            | LT | GT | EQ ->
                if t1 = TInt && t2 = TInt then TBool
                else raise(TypeError "Type error: binary comparison operations expect TInt operands")
            | AND | OR ->
                if t1 = TBool && t2 = TBool then TBool
                else raise(TypeError "Type error: binary logical operations expect TBool operands")
        )
    | UnaryOp(op, e) ->
        let t = check_expr env e in
        (
            match op with
            | NEG ->
                if t = TInt then TInt
                else raise(TypeError "Type error: unary arithmetic operations expect a TInt operand")
            | NOT ->
                if t = TBool then TBool
                else raise(TypeError "Type error: unary logical operations expect a TBool operand")
        )
    | ExprFuncCall(f, args) -> get_func_return_type env f args

let check_mult_expr env =function
    | StitchMultExpr(st, e) ->
        let t = check_expr env e in
        if t = TInt then TStitchSeqItem
        else raise(TypeError "Type error: stitch multiplier expression expects TInt")
    | StitchSeqMultExpr(seq, e) ->
        List.iter(fun item ->
            let t_item = check_stitch_seq_item env item in
            if t_item <> TStitchSeqItem then
                raise(TypeError "Type error: stitch sequence multiplier expression expects TStitchSeqItem values within parentheses")
        ) seq;
        let t_e = check_expr env e in
        if t_e = TInt then TStitchSeqItem
        else raise(TypeError "Type error: stitch sequence multiplier expression expects TInt")
and check_stitch_seq_item env = function
    | StitchSeqItem(mexpr) -> check_mult_expr env mexpr
    | StitchSeqItemVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TStitchSeqItem then TStitchSeqItem
        else raise(TypeError "Type error: variable '%s' expected TStitchSeqItem, but found '%s'" v (string_of_type t))
      )
and check_stitch_seq env = function
    | StitchSeq(seq) ->
        List.iter(fun item ->
            let t_item = check_stitch_seq_item env item in
            if t_item <> TStitchSeqItem then
                raise(TypeError "Type error: stitch sequence expects TSStitchSeqItem values")
        ) seq;
        TStitchSeq
    | StitchSeqVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TStitchSeq then TStitchSeq
        else raise(TypeError "Type error: variable '%s' expected TStitchSeq, but found '%s'" v (string_of_type t))
    )
    | StitchSeqFuncCall(f, args) ->  get_func_return_type env f args
and check_argument env = function
    | ExprArg(e) -> check_expr env e
    | StitchSeqArg(seq) -> check_stitch_seq env seq

let check_row_lit env = function
    | RowLit(e, seq) ->
        let t_e = check_expr env e in
        let t_seq = check_stitch_seq env seq in
        if t_e = TInt then
            if t_seq = TStitchSeq then TRow
            else raise(TypeError "Type error: row content expects TStitchSeq")
        else raise(TypeError "Type error: row number expects TInt")

let check_row_list_item env = function
    | RowLitItem(row) -> check_row_lit env row
    | RowVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TRow then TRow
        else raise(TypeError "Type error: variable '%s' expected TRow, but found '%s'" v (string_of_type t))
      )
    | RowFuncCall(f, args) -> get_func_return_type env f args

let check_definition env = function
    | ExprDef(v, e) -> 
        let t = check_expr env e in
        (v, t) :: env
    | StitchSeqDef(v, seq) -> 
        let t = check_stitch_seq env seq in (* TODO: is this part necessary, or does ast.ml enforce it as TStitchSeq *)
        (v, t) :: env
    | RowListDef(v, items) ->
        List.iter(fun item ->
            let t_item = check_row_list_item env item in
            if t_item <> TRow then
                raise(TypeError "Type error: row list definition expects TRow values")
        ) items;
        (v, TRowList) :: env
    | FuncCallDef(v, f, args) ->
        let return_type = get_func_return_type env f args in
        (v, return_type) :: env

let check_return_expr env = function
    | ReturnExpr(e) -> check_expr env e
    | ReturnStitchSeq(seq) -> check_stitch_seq env seq
    | ReturnRowList(items) ->
        List.iter(fun item ->
            let t_item = check_row_list_item env item in
            if t_item <> TRow then
                raise(TypeError "Type error: row list return expression expects TRow values")
        ) items;
        TRowList

let rec check_statement env = function
    | LetDef(def) -> check_definition env def (* returns a new env *)
    | Row(row) -> 
        let _ = check_row_lit env row in
        env
    | RowList(items) ->
        List.iter(fun item ->
            let t_item = check_row_list_item env item in
            if t_item <> TRow then
                raise(TypeError "Type error: row list statement expects TRow values")
        ) items;
        env
    | Return(ret_expr) ->
        let _ = check_return_expr env ret_expr in
        env
    | If(cond, then_branch, else_branch) ->
        let t_cond = check_expr env cond in
        if t_cond = TBool then
            let env_after_then = List.fold_left check_statement env then_branch in
            let env_after_else = List.fold_left check_statement env else_branch in
            get_env_intersection env_after_then env_after_else
        else
            raise(TypeError "Type error: if-else statement condition expects TBool")

let check_pattern_item env = function
    | FuncDef(f, params, body) -> 
        (* TODO: type inference to get param types and return type of function *)
        (* TODO: add function to main env *)
        (* TODO: create new environment for function env scope *)
        (* TODO: return back to original env scope after function returns *)
    | Stmt(stmt) -> (* TODO *)