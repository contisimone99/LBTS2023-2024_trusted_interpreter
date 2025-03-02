open Env
 
type expr =
  | CstInt of int
  | CstBool of bool
  | CstStr of string
  | CstList of expr list
  | BinOp of ide * expr * expr
  | Var of ide
  | Let of ide * expr * expr
  | If of expr * expr * expr
  | Fun of ide * expr
  | Call of expr * expr
  | Trust of expr * expr
  | Handle of expr
  | Include of ide * (value env -> value)
  | Exec of expr * expr list
  | Secret of string
 
and value =
  | Int of int
  | Bool of bool
  | String of string
  | ListVal of value list
  | Closure of ide * expr * value env
  | Taintedval of taintness
  | SecretVal of string
  
and taintness = taint * value
 
