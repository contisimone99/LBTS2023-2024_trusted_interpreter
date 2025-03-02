type ide = string
 
type 'v env = (ide * 'v) list
type taint = Tainted | Untainted
 
let empty_env = []
 
let rec lookup env id =
  match env with
  | [] -> failwith ("[ERROR]: Variable not found: " ^ id)
  | (name, value) :: rest ->
      if name = id then value else lookup rest id
 
let extend env id var_value =
  (id, var_value) :: env
 