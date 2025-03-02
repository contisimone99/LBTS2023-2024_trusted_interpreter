open Ast
 
let print_str str =
  print_endline str
 
let print_int value =
  print_string (string_of_int value);
  print_newline ()
 
let print_bool value =
  if value then
    print_string "true"
  else
    print_string "false";
  print_newline ()
 

let contains_substring str substr =
  let len_str = String.length str in
  let len_substr = String.length substr in
  let rec aux i =
    if i + len_substr > len_str then
      false
    else if String.sub str i len_substr = substr then
      true
    else
      aux (i + 1)
  in
  aux 0
let rec print_value value =
  match value with
    | Int n -> print_int n
    | Bool b -> print_bool b
    | String s -> print_str s
    | ListVal l -> print_endline ("[" ^ (String.concat "; " (List.map string_of_value l)) ^ "]")
    | Closure (_, _, _) -> print_endline "<closure>"
    | SecretVal v -> print_endline ("Secret: " ^ (string_of_value (String v)))
    | Taintedval v -> let (t1,v1) = v in 
      match t1 with
        |Tainted -> print_endline ("Tainted: " ^(string_of_value v1) )
        |Untainted ->  print_endline ("Tainted: " ^(string_of_value v1))
 
and string_of_value value =
  match value with
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | String s -> s
    | ListVal l -> "[" ^ (String.concat "; " (List.map string_of_value l)) ^ "]"
    | Closure (_, _, _) -> "<closure>"
    | SecretVal v -> "Trusted(" ^ (string_of_value (String v)) ^ ")"
    | Taintedval _ -> "<tainted>"

  (*function that given a CstList check if any of its values is a Secret value in order to prevent leakage of information*)
let rec check_secret lst =
  match lst with
    | [] -> false
    | hd::tl -> match hd with
        | SecretVal _ -> true
        | _ -> check_secret tl
