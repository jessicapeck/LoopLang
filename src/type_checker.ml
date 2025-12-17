exception TypeError of string
exception ArgError of string

let rec check_stitch env st =
    match st with
    | CH | SC | DC | INC | DEC -> TStitch

let rec check_expr env expr =
    match expr with
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
                else raise(TypeError "Type error: binary arithmetic operations expect integer operands")
            | LT | GT | EQ ->
                if t1 = TInt && t2 = TInt then TBool
                else raise(TypeError "Type error: binary comparison operations expect integer operands")
            | AND | OR ->
                if t1 = TBool && t2 = TBool then TBool
                else raise(TypeError "Type error: binary logical operations expect boolean operands")
        )
    | UnaryOp(op, e) ->
        let t = check_expr env e in
        (
            match op with
            | NEG ->
                if t = TInt then TInt
                else raise(TypeError "Type error: unary arithmetic operations expect an integer operand")
            | NOT ->
                if t = TBool then TBool
                else raise(TypeError "Type error: unary logical operations expect a boolean operand")
        )

let check_mult_expr env mexpr =
    match mexpr with
    | StitchMultExpr(st, e) ->
        (* st is enforced to be TStitch by ast.ml so don't need to check *)
        let t = check_expr env e in
        if t = TInt then TStitchSeqItem
        else raise(TypeError "Type error: stitch multiplier must be an integer")
    | StitchSeqMultExpr(seq, e) ->
        (* TODO: update seq checking to be for stitch_seq_item list *)
        List.iter(fun item ->
            let t_item = check_stitch_seq_item env item in
            if t_item <> TStitchSeqItem then
                raise(TypeError "Type error: stitch sequence multiplier must contain stitch sequence items")
        ) seq;
        let t_e = check_expr env e in
        if t_e = TInt then TStitchSeqItem
        else raise(TypeError "Type error: stitch sequence multiplier must be an integer")
and check_stitch_seq_item env item =
    match item with
    | StitchSeqItem(mexpr) -> check_mult_expr env mexpr
    | StitchSeqItemVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TStitchSeqItem then TStitchSeqItem
        else raise(TypeError "Type error: stitch sequence item variable has incorrect type")
      )

let check_stitch_seq env stitch_seq =
    match stitch_seq with
    | StitchSeq(seq) ->
        List.iter(fun item ->
            let t_item = check_stitch_seq_item env item in
            if t_item <> TStitchSeqItem then
                raise(TypeError "Type error: TODO")
        ) seq;
        TStitchSeq
    | StitchSeqVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TStitchSeq then TStitchSeq
        else raise(TypeError "Type error: stitch sequence variable has incorrect type")
    )

let check_argument env arg =
    match arg with
    | ExprArg(e) -> check_expr env e
    | StitchSeqArg(seq) -> check_stitch_seq env seq

let check_definition env def =
    match def with
    | ExprDef(v, e) -> 
        let t = check_expr env e in
        (v, t) :: env
    | StitchSeqDef(v, seq) -> 
        let t = check_stitch_seq env seq in (* TODO: is this part necessary, or does ast.ml enforce it as TStitchSeq *)
        (v, t) :: env
    | FuncCallDef(v, f, args) ->
        (* TODO: do lookup based on function name and arg num / arg types ??? *)
        let func_type =
            try List.assoc f env
            with Not_found -> raise(TypeError ("Undefined function: " ^ f))
        in
        let (param_types, return_type) = 
            match func_type with
            | TFunc(p, r) -> (p, r)
            | _ -> raise(TypeError "Type error: '" ^ f ^"' is not a function type")
        in
        (* check types of args match *)
        try
            List.iter2(fun param_type arg ->
                let t = check_argument env arg in
                if t <> param_type then raise(TypeError "Type error: argument type and expected parameter type do not match")
            ) param_types args;
        with Invalid_argument -> raise(ArgError "ArgError: number of arguments passed and number of parameters expected do not match");
        (v, return_type) :: env

let check_stmt_expr env stmt_expr =
    match stmt_expr with
    | Row(e, seq) ->
        let t_e = check_expr env e in
        let t_seq = check_stitch_seq seq in
        if t_e = TInt then
            if t_seq = TStitchSeq then env
            else raise(TypeError "Type error: row content must be a stitch sequence")
        else raise(TypeError "Type error: row number must be an integer")
    | FuncCall(f, args) ->
        let func_type =
            try List.assoc f env
            with Not_found -> raise(TypeError ("Undefined function: " ^ f))
        in
        let (param_types, return_type) = 
            match func_type with
            | TFunc(p, r) -> (p, r)
            | _ -> raise(TypeError "Type error: '" ^ f ^"' is not a function type")
        in
        (* check types of args match *)
        try
            List.iter2(fun param_type arg ->
                let t = check_argument env arg in
                if t <> param_type then raise(TypeError "Type error: argument type and expected parameter type do not match")
            ) param_types args;
            env
        with Invalid_argument -> raise(ArgError "ArgError: number of arguments passed and number of parameters expected do not match")
