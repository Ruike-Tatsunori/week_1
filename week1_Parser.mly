%{
  open Week1_syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)

  (* 目的：変数列と本体の式から、入れ子になった１引数関数を作る *)
  (* create_fun : string list -> Syntax.t -> Syntax.t *)
  (*let create_fun variables expr =
    List.fold_right (fun var expr -> EFun (var, expr)) variables expr

  (* 目的：式の列から、Cons を使ったリストを作る *)
  (* create_list : Syntax.t list -> Syntax.t *)
  let create_list exprs =
    List.fold_right (fun expr lst -> Cons (expr, lst)) exprs Nil*)
%}

/* トークンの定義 */
%token <int>    INT
%token ADD SUB MUL DIV
%token <bool>   BOOL
%token LT EQ
%token IF THEN ELSE
%token <string> ID
%token LET REC IN FUN ARROW
%token LBRACKET RBRACKET MATCH WITH CONS
%token LPAR RPAR
%token SEMI
%token EOF 

/* 非終端記号の定義 */
%type <Week1_syntax.expr> main
/* エントリーポイント(開始記号)の定義 */
%start main 

/* 結合優先順位の定義 */
%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%nonassoc ARROW
%nonassoc EQ LT
%right    CONS
%left     ADD SUB
%left     MUL DIV
%nonassoc UNARY
%%

main: 
command EOF {$1}
;

command:
  | expr                 { $1 }
  //| LET ID EQ expr { CLet($2,$4) }
;

expr:
  //| LET ID EQ expr IN expr                     { ELet($2,$4,$6) }
  //| LET ID variables EQ expr IN expr           { ELet($2,create_fun $3 $5,$7)}
  //| LET REC ID ID EQ expr IN expr              { ELR($3,$4,$6,$8)}
  //| LET REC ID ID variables EQ expr IN expr    { ELR($3, $4, create_fun $5 $7, $9) }
  | IF expr THEN expr ELSE expr                { EIf($2,$4,$6) }
  //| FUN variables ARROW expr                   { create_fun $2 $4 }
  | arith_expr                                 { $1 } 
  //| app                                        { $1 }
  //| expr CONS expr                             { Cons($1,$3)}
;

arith_expr:
  | arith_expr ADD arith_expr { EBin(OpAdd,$1,$3) }
  | arith_expr SUB arith_expr { EBin(OpSub,$1,$3) }
  | arith_expr MUL arith_expr { EBin(OpMul,$1,$3) }
  | arith_expr DIV arith_expr { EBin(OpDiv,$1,$3) }
  | arith_expr EQ  arith_expr { EBin(OpEQ,$1,$3) }
  | arith_expr LT  arith_expr { EBin(OpLT,$1,$3) }
  | factor_expr               { $1 }
;

factor_expr:
  | INT                              { EValue(VInt $1) }
  | BOOL                             { EValue(VBool $1) }
  //| ID                               { EVar($1) }
  | LPAR expr RPAR                   { $2 }
  //| LBRACKET RBRACKET                { Nil }
  //| LBRACKET expr_semi_list RBRACKET { create_list $2 }
;
/*
variables:
  | ID  { [$1] }
  | ID variables { $1 :: $2 }  
;

app:
  //| factor_expr factor_expr { EApp($1,$2) }
  //| app factor_expr         { EApp($1,$2) }
;

expr_semi_list:
| expr opt_semi
        { [$1] }
| expr SEMI expr_semi_list
        { $1 :: $3 }
;

opt_semi:
|       { () }
| SEMI  { () }
;*/


 

