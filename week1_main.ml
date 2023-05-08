open Week1_syntax

let go_rep () =
  let e = Week1_Parser.main Week1_Lexer.token (Lexing.from_string (read_line ())) in
  (* これで標準入力を字句解析して、構文解析した結果を e に入れ *)
  print_string "Result : ";
  let result = Week1_eval.eval e in print_value result;
  print_newline ();;

let go_file inchannel =
  let e = Week1_Parser.main Week1_Lexer.token (Lexing.from_channel inchannel) in
  (* これで標準入力を字句解析して、構文解析した結果を e に入れ *)
  print_string "Result : ";
  let result = Week1_eval.eval e in print_value result;
  print_newline ();;

let read_eval_print () = 
  let rec rep () = go_rep (); rep ()
    in rep ()

let read_file : string -> in_channel = 
  fun filename -> open_in filename

let file_eval filename = 
  if Sys.file_exists (filename) 
    then go_file (read_file filename)
    else raise (Sys_error "File not found") 

(*let _ = read_eval_print ()*)

(* スタートアップ*)
let _ = if Sys.argv.(1) <> ""
          then file_eval Sys.argv.(1)
          else read_eval_print ()

(*
let main () =
  try 
    let lexbuf = Lexing.from_channel stdin in 
    let result = Week1_Parser.main Week1_Lexer.token lexbuf in
    print_string "Parsed : ";
    print_expr result;            (* 入力を表示する *)
    print_newline ();
    print_string "Result : ";
    print_value (Week1_eval.eval result); 
    print_newline ()
  with 
    | Parsing.Parse_error -> 
      print_endline "Parse Error!"
      
;;
if !Sys.interactive then 
  ()
else 
  main ()    
  *)