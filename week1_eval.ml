open Week1_syntax

let rec apply_prim op value_1 value_2 =
  match (op, value_1, value_2) with
  (OpAdd, VInt i1, VInt i2) -> VInt (i1 + i2) |
  (OpAdd, _, _) -> raise (Invalid_argument "Eval_error:ADD is included boolean.") |
  (OpSub, VInt i1, VInt i2) -> VInt (i1 - i2) |
  (OpSub, _, _) -> raise (Invalid_argument "Eval_error:SUB is included boolean.") |
  (OpMul, VInt i1, VInt i2) -> VInt (i1 * i2) |
  (OpMul, _, _) -> raise (Invalid_argument "Eval_error:MUL is included boolean.") |
  (OpDiv, VInt i1, VInt i2) -> if i2 = 0 then raise (Invalid_argument "Eval_error_Not dividible by zero") else VInt (i1 / i2) |
  (OpDiv, _, _) -> raise (Invalid_argument "Eval_error:DIV is included boolean.") |
  (OpEQ, VInt i1, VInt i2) -> VBool (i1 = i2) |
  (OpEQ, VBool i1, VBool i2) -> VBool (i1 = i2) |
  (OpEQ, _, _) -> raise (Invalid_argument "Eval_error_EQ:Each types are different.") |
  (OpLT, VInt i1, VInt i2) -> VBool (i1 < i2) |
  (OpLT, _, _) -> raise (Invalid_argument "Eval_error_LT:Each types are different.");;

let rec eval : expr -> value = fun exp -> 
  match exp with
    EValue i -> i
  | EBin (op, eva1, eva2) -> apply_prim op (eval eva1) (eval eva2) 
  | EIf (eva1, eva2, eva3) ->
      let judgement = eval eva1 in
        (match judgement with
           VBool true -> eval eva2 
         | VBool false -> eval eva3
         | _ -> raise (Invalid_argument "Eval_error"));;

(*let rec eval_2 : command -> value *)