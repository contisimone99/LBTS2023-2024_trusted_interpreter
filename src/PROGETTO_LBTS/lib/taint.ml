open Ast


let is_tainted (t : taintness) : bool =
  match t with
    | Tainted, _ -> true
    | Untainted, _ -> false

let mark_tainted (v : value) : taintness =
  (Tainted, v)

let add (t1 : taintness) (t2 : taintness) : taintness =
  let (taintness_1, v1) = t1 in
  let (taintness_2, v2) = t2 in
  let sum = 
    (match v1 with 
      | Int n -> n
      | _ -> failwith "[ERROR]: Value 1 in add must be integer") +
    (match v2 with 
      | Int n -> n 
      | _ -> failwith "[ERROR]: Value 2 in add must be integer") 
    in
    if taintness_1 = Tainted || taintness_2 = Tainted then
      (Tainted, Int sum)
    else
      (Untainted, Int sum)

let sub (t1 : taintness) (t2 : taintness) : taintness =
  let (taintness_1, v1) = t1 in
  let (taintness_2, v2) = t2 in
  let diff = 
    (match v1 with 
      | Int n -> n
      | _ -> failwith "[ERROR]: Value 1 in sub must be integer") -
    (match v2 with 
      | Int n -> n 
      | _ -> failwith "[ERROR]: Value 2 in sub must be integer") 
    in
    if taintness_1 = Tainted || taintness_2 = Tainted then
      (Tainted, Int diff)
    else
      (Untainted, Int diff)

let mul (t1 : taintness) (t2 : taintness) : taintness =
  let (taintness_1, v1) = t1 in
  let (taintness_2, v2) = t2 in
  let prod = 
    (match v1 with 
      | Int n -> n
      | _ -> failwith "[ERROR]: Value 1 in mul must be integer") *
    (match v2 with 
      | Int n -> n 
      | _ -> failwith "[ERROR]: Value 2 in mul must be integer") 
    in
    if taintness_1 = Tainted || taintness_2 = Tainted then
      (Tainted, Int prod)
    else
      (Untainted, Int prod)

let div (t1 : taintness) (t2 : taintness) : taintness =
  let (taintness_1, v1) = t1 in
  let (taintness_2, v2) = t2 in
  let quot = 
    (match v1 with 
      | Int n -> n
      | _ -> failwith "[ERROR]: Value 1 in div must be integer") /
    (match v2 with 
      | Int n -> n 
      | _ -> failwith "[ERROR]: Value 2 in div must be integer") 
    in
    if taintness_1 = Tainted || taintness_2 = Tainted then
      (Tainted, Int quot)
    else
      (Untainted, Int quot)


