type token =
  | INT of (int)
  | ADD
  | SUB
  | MUL
  | DIV
  | BOOL of (bool)
  | LT
  | EQ
  | IF
  | THEN
  | ELSE
  | ID of (string)
  | LET
  | REC
  | IN
  | FUN
  | ARROW
  | LBRACKET
  | RBRACKET
  | MATCH
  | WITH
  | CONS
  | LPAR
  | RPAR
  | SEMI
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Week1_syntax.expr
