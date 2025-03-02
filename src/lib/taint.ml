open Ast


let is_tainted (t : taintness) : bool =
  match t with
    | Tainted, _ -> true
    | Untainted, _ -> false

let mark_tainted (v : value) : taintness =
  (Tainted, v)

