{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIVISION }
  | "*" { MULTIPLY }
  | num { CONST (Lexing.lexeme lexbuf) }
  | "0x"{ HEX }
  | eof { EOF }
