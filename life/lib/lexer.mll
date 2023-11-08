{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'8']

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "/B" { DIVIDER }
  | "S" { START }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
