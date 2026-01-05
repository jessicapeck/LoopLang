open Ast

let update_ctx ctx name expected_type =
  match Hashtbl.find_opt ctx name with
  | Some(None) -> Hashtbl.replace ctx name (Some expected_type)
  | Some(Some(t)) -> if t <> expected_type then raise(TypeError "inconsistent type inference for parameter '" ^ name ^ "'")
  | None -> ()


let infer_statement env ctx stmt =
  match stmt with
  | LetDef(def) -> (* TODO *)
  | Row(row) -> (* TODO *)
  | RowList(rows) -> (* TODO *)
  | Return(ret_expr) -> (* TODO *)
  | If(cond, then_branch, else_branch) -> (* TODO *)


let infer_func_type f params body =
  (* initialise local env with params of unknown types *)
  let ctx = Hashtbl.create 10 in
  List.iter (fun param -> Hashtbl.add ctx param None) params;

  let rec analyse_body local_env return_types body =
    match body with
    | [] -> return_types
    | stmt :: stmts ->
        let new_local_env, new_return_types = infer_statement local_env ctx stmt
        in analyse_body new_local_env (return_types @new_return_types) stmts

  (* analyse body to determine return types *)
  let all_return_types = analyse_body [] [] body in

  (* check return types are consistent to find overall return type *)
  let return_type = 
    match all_return_types with
    | [] -> raise (TypeError "function '" ^ f ^ "' does not return a value")
    | t :: ts ->
        List.iter (fun t' ->
          if t' <> t then
            raise (TypeError "function '" ^ f ^ "' has inconsistent return types")
        ) ts;
        t
  in

  (* get parameter types from context *)
  let param_types = List.map(fun param ->
    match Hashtbl.find ctx param with
    | Some t -> t
    | None -> raise (TypeError ("unable to infer type of parameter '" ^ param ^ "' in function '" ^ f ^ "'"))
  ) params in

  (* return parameter types and return type *)
  (param_types, return_type)



