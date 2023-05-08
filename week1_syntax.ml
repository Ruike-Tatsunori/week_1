type value =
  | VInt  of int
  | VBool of bool

type binOp =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEQ
  | OpLT

type expr =
  | EVar of string
  | EValue of value 
  | EBin   of binOp * expr * expr 
  | EIf    of expr * expr * expr

type command = 
  | CExp of expr
  | CLet of string * expr

(*type command_result = 
  Commnad_result of string option * value * env*)

let print_name = print_string 

let print_value = function 
  | VInt i -> print_int i
  | VBool b -> print_string (string_of_bool b)

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)
let rec print_expr = function
  | EVar s -> print_name s
  | EValue v -> 
    print_value v 
  | EBin (bin,e1,e2) -> 
    (print_string "EBin (";
     begin match bin with 
      | OpAdd -> print_string "OpAdd"
      | OpSub -> print_string "OpSub"
      | OpMul -> print_string "OpMul"
      | OpDiv -> print_string "OpDiv"
      | OpEQ  -> print_string "OpEQ"
      | OpLT  -> print_string "OpLT"
     end; 
     print_string ",";
     print_expr e1;
     print_string ",";
     print_expr e2;
     print_string ")")
  | EIf (e1,e2,e3) ->
    (print_string "EIf (";
     print_expr e1;
     print_string ",";
     print_expr e2;
     print_string ",";
     print_expr e3;
     print_string ")")
 (* | ELet (n,e1,e2) ->
    (print_string "ELet (";
     print_name n;
     print_string ","; 
     print_expr e1;
     print_string ",";
     print_expr e2;
     print_string ")")

let rec print_command = function 
  | CExp exp -> 
    print_expr exp
  | CLet (n, exp) -> 
    (print_string "CLet (";
     print_name n;
     print_string ","; 
     print_expr exp;
     print_string ")")
    *)
       


    
