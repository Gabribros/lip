{
open Parser
}

let white = [' ' '\t' '\n']+

let const = ['1'-'9']['0'-'9']* | '0'

let variable = ['a'-'z']*

rule read =
  parse
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }  
  | "then" { THEN }
  | "else" { ELSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF }
  | "while" { WHILE }
  | "do" { DO }
  | white { read lexbuf }
  | const { CONST(int_of_string (Lexing.lexeme lexbuf))}
  | variable { VAR(Lexing.lexeme lexbuf) }
  | eof { EOF }
