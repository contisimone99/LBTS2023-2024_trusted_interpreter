open PROGETTO_LBTS.Ast
open PROGETTO_LBTS.Env
open PROGETTO_LBTS.Interpreter
open PROGETTO_LBTS.Taint
open PROGETTO_LBTS.Utilities


let test_addition() =
  let empty_env = empty_env in
  let addition_expr = BinOp ("+", CstInt 74, CstInt 32) in
  let addition_result = eval empty_env false addition_expr in
  assert (addition_result = Int 106);;


let test_multiplication() =
  let empty_env = empty_env in
  let multiplication_expr = BinOp ("*", CstInt 18, CstInt 21) in
  let multiplication_result = eval empty_env false multiplication_expr in
  assert (multiplication_result = Int 378);;


let test_boolean() =
  let empty_env = empty_env in
  let bool_expr_true = CstBool true in
  let bool_result_true = eval empty_env false bool_expr_true in
  assert (bool_result_true = Bool true);;


let test_conditional() =
  let empty_env = empty_env in
  let conditional_expr = If (CstBool true, CstInt 10, CstInt 5) in
  let conditional_result = eval empty_env false conditional_expr in
  assert (conditional_result = Int 10);;


let test_concatenation() =
  let empty_env = empty_env in
  let concat_expr = BinOp ("+", CstStr "lb", CstStr "ts") in
  let concat_result = eval empty_env false concat_expr in
  assert (concat_result = String "lbts");;
  

let test_subtraction() =
  let empty_env = empty_env in
  let subtraction_expr = BinOp ("-", CstInt 3, CstInt 5) in
  (try
     let _ = eval empty_env false subtraction_expr in
     assert false
   with
    | _ -> assert true);;


let test_if() =
  let empty_env = empty_env in
  let if_expr = If (CstBool true, CstInt 10, CstInt 5) in
  let if_result = eval empty_env false if_expr in
  assert (if_result = Int 10);;
  

let test_else() =
  let empty_env = empty_env in
  let else_expr = If (CstBool false, CstInt 10, CstInt 5) in
  let else_result = eval empty_env false else_expr in
  assert (else_result = Int 5);;


let test_taint_analysis() =
  let t1 = (Untainted, Int 15) in
  let t2 = (Tainted, Int 100) in
  let tainted_result = add t1 t2 in
  assert (tainted_result = (Tainted, Int 115));

  let t3 = (Untainted, Int 12) in
  let untainted_result = add t1 t3 in
  assert (untainted_result = (Untainted, Int 27));

  let t4 = (Untainted, Int 15) in
  let tainted_result2 = sub t2 t4 in
  assert (tainted_result2 = (Tainted, Int 85));

  let t5 = (Tainted, Int 20) in
  let tainted_result3 =  mul t2 t5 in
  assert (tainted_result3 = (Tainted, Int 2000));

  let t6 = (Untainted, Int 20) in
  let tainted_result4 = div t6 t4 in
  assert (tainted_result4 = (Untainted, Int 1));;
  
  
let test_plugin() =
  let myFilter = Include ("filter", fun env -> Closure ("x", Var "x", env)) in
  let even = Fun ("n", BinOp ("=", BinOp ("%", Var "n", CstInt 2), CstInt 0)) in
  let filter_expr = Exec (myFilter, [even; CstList [CstInt 1; CstInt 2; CstInt 3; CstInt 4]]) in
  let filter_result = eval empty_env false filter_expr in
  assert (filter_result = ListVal [Int 2; Int 4]);
  let lst = Exec (myFilter, [Fun ("x", If (BinOp ("=", Var "x", CstStr "abcd"), CstBool true, CstBool false)); CstList [CstStr "abcd"; CstStr "efgh"]]) in
  let lst_result = eval empty_env false lst in
  assert (lst_result = ListVal [String "abcd"]);;    (* Expected output: [abcd] *)


let test_execute() =
  (* test that exec is blocked if we are inside a trust context from a trust block*)
  let myFilter = Include ("filter", fun env -> Closure ("x", Var "x", env)) in
  let even = Fun ("n", BinOp ("=", BinOp ("%", Var "n", CstInt 2), CstInt 0)) in
  let tainted_filter_expr = Exec (myFilter, [even; CstList [CstInt 1; CstInt 2; CstInt 3; CstInt 4]]) in
  
  (* test that exec is blocked if we are inside a trusted context*)
  (try 
    let _ = eval empty_env true tainted_filter_expr in
    assert false (* Should never reach here *)
  with
    | Failure msg -> assert (contains_substring msg "[ERROR]: Plugins are not allowed in trust context"));
  
  (* test if exec can execute outside of a trusted context*)
  let tainted_lst = Exec (myFilter, [Fun ("x", If (BinOp ("=", Var "x", CstStr "abcd"), CstBool true, CstBool false)); CstList [CstStr "abcd"; CstStr "efgh"]]) in
  let tainted_lst_result = eval empty_env false tainted_lst in
  assert(tainted_lst_result = ListVal [String "abcd"])
  

let test_trustblock_untainted() = 
(* Trusted expression that does not accept negative values *)
  try
    let trusted_expr_value = CstInt (42) in
    if (match trusted_expr_value with CstInt n -> n < 0 | _ -> false) then
      failwith "Negative values not allowed in trusted expressions"
    else
      let trusted_expr = Let ("x", trusted_expr_value, Var "x") in

      let initial_balance = CstInt 100 in
      (* let withdraw = CstInt 200 in *)

      (* Define a function that performs addition *)
      let withdraw = Fun ("y", BinOp ("-", initial_balance, Var "y")) in

      (* Define a function that concatenates strings *)
      let concat_fun = Fun ("str", BinOp ("+", Var "str", CstStr " balance!")) in

      (* Define the body that calls these functions *)
      let body = Let ("balance", Call (withdraw, trusted_expr),
                      Let ("concatenated", Call (concat_fun, CstStr "refreshed"),
                            BinOp ("-", Var "balance", CstInt 0))) in

      (* Define the Trust block *)
      let trust_block = Trust (trusted_expr, body) in

      (* Evaluate the Trust block *)
      let trust_result = eval empty_env false trust_block in
        assert (trust_result = Int 58)
  with
    | _ -> assert false;;


  let test_trustblock_plugin() =
    let myFilter = Include ("filter", fun env -> Closure ("x", Var "x", env)) in
    let even = Fun ("n", BinOp ("=", BinOp ("%", Var "n", CstInt 2), CstInt 0)) in
    let filter_expr = Exec (myFilter, [even; CstList [CstInt 1; CstInt 2; CstInt 3; CstInt 4]]) in
    let filter_result = eval empty_env false filter_expr in
    assert (filter_result = ListVal [Int 2; Int 4]);;


  let test_nested_trust_block() =
    try
      let env = empty_env in
      let nested_trust = Trust (CstInt 5, CstStr "nested trust") in
      let trust_block = Trust (nested_trust, CstStr "outer trust") in
      (* Evaluate the expression, but discard the result since we only care about the potential error *)
      let _ = eval env false trust_block in
      assert false; (* Should never reach here *)
    with
      | Failure msg -> assert (contains_substring msg "[ERROR]: Trust block cannot be declared inside a trust block");;


  let test_handle_within_exec() =
    try
      let env = empty_env in
      let exec_call = Exec (Fun ("x", CstStr "exec dentro handle"), []) in
      let inner_handle = Handle (exec_call) in
      let _ = eval env true inner_handle in 
      assert false; (* Should never reach here *)
    with
      | Failure msg -> assert (contains_substring msg "[ERROR]: Handle block cannot contain an Exec call");;
         

  let test_handle_outside_trust() =
    try
      let env = empty_env in
      let handle_expr = Handle (CstStr "handle expression") in
      let _ = eval env false handle_expr in
      assert false; (* Should never reach here *)
    with
      | Failure msg -> assert (contains_substring msg "[ERROR]: Handle block cannot be called outside a trust block");;

      
  let test_trustblock_secret() =
    let secret = CstStr "abcd" in
    let guess = Let ("guess", CstStr "abcd", Var "guess") in
    let checkPwd = Fun ("guess", BinOp ("=", guess, secret)) in
    let handle_expr = Handle (checkPwd) in
    let trust_block = Trust (secret, handle_expr) in
    let trust_result = eval empty_env false trust_block in
    assert (trust_result = Bool true);
    
    (*check that if we try to leak the secret of the trust block by using the plugin filter it fails*)
    let myFilter = Include ("filter", fun env -> Closure ("x", Var "x", env)) in
    let filter_expr = Exec (myFilter, [checkPwd; CstList [Secret "abcd"; CstStr "efgh"]]) in
    (try
      let _ = eval empty_env false filter_expr in
      assert false
    with
      | Failure msg -> assert (contains_substring msg "[Blocked]: Prevention of secret data leakage"));;

    

let () =
  Printf.printf "***** PRIMITIVE TESTS *****\n";
    test_addition();
    Printf.printf "test addition Passed\n";
    test_multiplication ();
    Printf.printf "test multiplication Passed\n";
    test_boolean();
    Printf.printf "test boolean Passed\n";
    test_conditional();
    Printf.printf "test conditional Passed\n";
    test_concatenation ();
    Printf.printf "test concatenation Passed\n";
    test_subtraction();
    Printf.printf "***** All PRIMITIVE TESTS PASSED! *****\n\n";
  
  Printf.printf "***** IF TESTS *****\n";
    test_if();
    Printf.printf "test if Passed\n";
    test_else();
    Printf.printf "test else Passed\n";
    Printf.printf "***** All IF TESTS PASSED! *****\n\n";
  
  Printf.printf "***** TAINTNESS TESTS *****\n";
    test_taint_analysis();
    Printf.printf "test taint analysis Passed\n";
    Printf.printf "***** All TAINTNESS TESTS PASSED! *****\n\n";

  Printf.printf "***** EXECUTE TEST *****\n";
    test_execute();  
    Printf.printf "test execute Passed\n";
    Printf.printf "***** All EXECUTE TESTS PASSED! *****\n\n";

  Printf.printf "***** PLUGIN TEST *****\n";
    test_plugin();
    Printf.printf "test plugin Passed\n";
    Printf.printf "***** All PLUGIN TESTS PASSED! *****\n\n";
    
  Printf.printf "***** TRUSTBLOCK TESTS *****\n";
    test_trustblock_untainted();
    Printf.printf "test trustblock untainted Passed\n";
    test_nested_trust_block();
    Printf.printf "test nested trust block Passed\n";
    test_handle_within_exec();
    Printf.printf "test handle within exec Passed\n";
    test_handle_outside_trust();
    Printf.printf "test handle outside trust Passed\n";
    test_trustblock_plugin();
    Printf.printf "test trustblock plugin Passed\n";
    Printf.printf "***** All TRUSTBLOCK TESTS PASSED! *****\n\n";
    Printf.printf "***** Secrets Tests *****\n";
    test_trustblock_secret();
    Printf.printf "test trustblock secret Passed\n";
    Printf.printf "***** All Secrets Tests PASSED! *****\n\n";;
