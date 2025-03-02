open Ast
open Env
open Utilities

let rec eval env trust_ctx expr =
  match expr with
  | CstInt n -> Int n
  | CstBool b -> Bool b
  | CstStr s -> String s
  | CstList l -> ListVal (List.map (eval env trust_ctx) l)

  | BinOp (op, e1, e2) -> (
    let v1 = eval env trust_ctx e1 in
    let v2 = eval env trust_ctx e2 in
    begin
      (match (v1, v2) with
        | (Int n1, Int n2) -> (
          let res =
            match op with
              | "+" -> Int (n1 + n2)
              | "-" -> if n1 < n2 then failwith "[ERROR]: Negative results are forbidden" else Int (n1 - n2)
              | "/" -> if n2 = 0 then failwith "[ERROR]: Division by zero not allowed" else Int (n1 / n2)
              | "*" -> Int (n1 * n2)
              | "=" -> Bool (n1 = n2)
              | "%" -> Int (n1 mod n2)
              | _ -> failwith ("[ERROR]: Operation not recognized: " ^ op)
            in
            match v1, v2 with
              | Taintedval _, _ | _, Taintedval _ -> Taintedval (Tainted, res)
              | _ -> res)
        | (String s1, String s2) ->(
          let res =
            match op with
              | "=" -> Bool (s1 = s2)
              | "+" -> String (s1 ^ s2)
              | _ -> failwith ("[ERROR]: Unknown primitive operation for strings: " ^ op)
            in
            match v1, v2 with
              | Taintedval _, _ | _, Taintedval _ -> Taintedval (Tainted, res)
              | _ -> res)
        | (Bool b1, Bool b2) ->(
          let res =
            match op with
              | "||" -> Bool (b1 || b2)
              | "&&" -> Bool (b1 && b2)
              | _ -> failwith "[ERROR]: Unknown operator or wrong types for operation"
            in
            match v1, v2 with
              | Taintedval _, _ | _, Taintedval _ -> Taintedval (Tainted, res)
              | _ -> res)
        | _ -> failwith ("[ERROR]: Type error in primitive operation: " ^ op))
    end
  )

  | Var v -> (
    let var_val = lookup env v in
    if trust_ctx then
      match var_val with
        | Taintedval _ -> failwith "[ERROR]: Attempted access to tainted value within trust block"
        | _ -> var_val
    else
      var_val
  )

  | Let (id, val_expr, body_expr) -> (
    let let_value = eval env trust_ctx val_expr in
    let new_env = extend env id let_value in
    (* Propagate taint to the newly created environment *)
    let new_env = 
      match let_value with
        | Taintedval _ -> extend new_env "let_result" (Taintedval (Tainted, let_value))
        | _ -> new_env
      in
      eval new_env trust_ctx body_expr
  )
  
  | If (cond, e1, e2) -> (
    let v_cond = eval env trust_ctx cond in
    begin
      match v_cond with
        | Bool true -> eval env trust_ctx e1
        | Bool false -> eval env trust_ctx e2
        | _ -> failwith "[ERROR]: Condition of If must be of a boolean value"
    end
  )

  | Fun (param, fbody) -> Closure (param, fbody, env)
  
  | Call (eFun, eArg) -> (
    let fCall = eval env trust_ctx eFun in
    let arg_val = eval env trust_ctx eArg in
    begin
      match fCall with
      | Closure (param, fBody, fEnv) ->
        (* Propagate taint to function arguments *)
        let new_env =
          match arg_val with
          | Taintedval _ -> extend fEnv param (Taintedval (Tainted, arg_val))
          | _ -> extend fEnv param arg_val
        in
        eval new_env trust_ctx fBody
      | _ -> failwith "[ERROR]: Expected a closure in Call"
    end
  )

  | Secret s -> SecretVal s

  | Trust (tCode, tBody) -> (
    if trust_ctx then
      failwith "[ERROR]: Trust block cannot be declared inside a trust block"
    else
      (* Check if the value provided in the trust block is tainted *)
      let tVal = eval env true tCode in
      let new_env = match tVal with
        | SecretVal s -> extend env "trusted" (SecretVal s)
        | Taintedval _ -> failwith "[ERROR]: Tainted value provided in a trust block"
        | _ -> extend env "trusted" tVal
      in
      let res = eval new_env true tBody in
      (* Check if the value returned from the trust block is tainted *)
      match res with
        | Taintedval _ -> Taintedval (Tainted, res)
        | _ -> res
  )

  | Handle expr -> (
    if not trust_ctx then
      failwith "[ERROR]: Handle block cannot be called outside a trust block"
    else
      match expr with
        | Exec (_,_) -> failwith "[ERROR]: Handle block cannot contain an Exec call"
        | _ -> let hVal = eval env trust_ctx expr in
          match hVal with
            | Closure (_, body, fEnv) ->
              (* Recursively evaluate the closure body within the closure's environment *)
              eval fEnv trust_ctx body
            | _ -> failwith "[ERROR]: Expected a closure in handle"
  )
        
  | Include (_, code) ->
    if trust_ctx then
      failwith "[ERROR]: Include block cannot be called inside a trust block"
    else
      code env
  | Exec (plugin, args) ->
    if trust_ctx then
      failwith "[ERROR]: Plugins are not allowed in trust context"
    else
       let pVal = eval env trust_ctx plugin in
       let arg_val = List.map (eval env trust_ctx) args in
      
       (match pVal with
        | Closure (_, _, _) ->
          (match arg_val with
            | [fFilter; ListVal lst] ->
              (*check if lst has any secret value inside *)
                if check_secret lst then
                  failwith "[Blocked]: Prevention of secret data leakage"
                else
                (match fFilter with
                  | Closure (param, body, fEnv) ->
                    filter lst param body fEnv trust_ctx
                  | _ -> failwith "[ERROR]: Expected a closure for filter function")
            | _ -> failwith "[ERROR]: Expected filter function and list as arguments")
        | _ -> failwith "[ERROR]: Expected a plugin for execution") 
        

and env_string env =
  let binds = List.map (fun (name, value) -> name ^ " = " ^ (value_string value)) env in
  "{" ^ (String.concat "; " binds) ^ "}"

and value_string value =
  match value with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | String s -> s
  | ListVal l -> "[" ^ (String.concat "; " (List.map value_string l)) ^ "]"
  | Closure (_, _, _) -> "<closure>"
  | Taintedval (_, v) -> "Tainted(" ^ (value_string v) ^ ")"
  | SecretVal _ -> "<secret>"

  and filter lst param body fEnv trust_ctx =
    match lst with
    | [] -> ListVal []
    | h :: t ->
        let cond = eval (extend fEnv param h) trust_ctx body in
        (match cond with
        | Bool true ->
            (match filter t param body fEnv trust_ctx with
            | ListVal t_filtered -> ListVal (h :: t_filtered)
            | _ -> raise (Failure "Expected a ListVal result from filter function"))
        | Bool false -> filter t param body fEnv trust_ctx
        | _ -> raise (Failure ("Expected a boolean result in filter function, but got " ^ (string_of_value cond))))