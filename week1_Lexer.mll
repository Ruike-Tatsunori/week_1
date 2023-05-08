{
  open Week1_Parser
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let boolean = "true" | "false"
let ident = alpha (alpha | digit)*

rule token = parse
| space+       { token lexbuf }
| "(*"         { comment lexbuf;
                 token lexbuf }
(*| "let"        { LET }
| "rec"        { REC } 
| "in"         { IN }*)
| "if"         { IF}
| "then"       { THEN }
| "else"       { ELSE }
(*| "fun"        { FUN }
| "match"      { MATCH }
| "with"       { WITH }
| "end"        { END }
| "["          { LBRACKET }
| "]"          { RBRACKET }
| "::"         { CONS }
| "|"          { BAR }*)
| "->"         { ARROW }
| "="          { EQ }
| "<"          { LT }
| '+'          { ADD }
| '-'          { SUB }
| '*'          { MUL }
| '/'          { DIV }
| '('          { LPAR }
| ')'          { RPAR }
(*| ";;"         { DSEMI }
| ';'          { SEMI }*)
| digit+ as n  { INT (int_of_string n) }
| boolean as n { BOOL (bool_of_string n) }
| ident  as n  { ID n }
| eof          { EOF }
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}


and comment = parse
| "*)"         { () }
| "(*"         { comment lexbuf;
                 comment lexbuf }
| eof          { failwith "unterminated comment" }
| _            { comment lexbuf }