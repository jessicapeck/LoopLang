exception TypeError of string

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
        let t_seq = check_stitch_seq env seq in
        let t_e = check_expr env e in
        if t_seq = TStitchSeq then 
            if t_e = TInt then
                TStitchSeqItem
            else raise(TypeError "Type error: stitch sequence multiplier must be an integer")
        else raise(TypeError "TODO")
and check_stitch_seq_item env item =
    match item with
    | StitchSeqItem(mexpr) -> check_mult_expr env mexpr
    | StitchSeqItemVar(v) -> (
        let t =
            try List.assoc v env
            with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
        in
        if t = TStitchSeqItem then TStitchSeqItem
        else raise(TypeError "TODO")
      )
and check_stitch_seq env stitch_seq =
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
        else raise(TypeError "TODO")
    )


