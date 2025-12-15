exception TypeError of string

let rec check_type env expr =
  match expr with
  | IntLit(_) -> TInt
  | BoolLit(_) -> TBool
  | Var(v) -> (
      try List.assoc v env
      with Not_found -> raise (TypeError ("Undefined variable: " ^ v))
    )
  | BinOp(e1, op, e2) ->
      let t1 = check_type env e1 in
      let t2 = check_type env e2 in
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
      let t = check_type env e in
      (
        match op with
        | NEG ->
            if t = TInt then TInt
            else raise(TypeError "Type error: unary arithmetic operations expect an integer operand")
        | NOT ->
            if t = TBool then TBool
            else raise(TypeError "Type error: unary logical operations expect a boolean operand")
      )

