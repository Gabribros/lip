open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option

let string_of_result n = match n with
| None -> "Division by zero"
| Some n -> string_of_int n
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) -> Some n
  | Negate(e1) -> Some( - Option.get(eval e1))
  | Add(e1,e2) -> Some ( Option.get(eval e1) + Option.get(eval e2) );
  | Minus(e1,e2) -> Some ( Option.get(eval e1) - Option.get(eval e2));
  | Division(_,e2) when(Option.get(eval e2)  = 0) -> None;		
  | Division(e1,e2) -> Some ( Option.get(eval e1 ) / Option.get(eval e2 ));
  | Multiply(e1,e2) -> Some ( Option.get(eval e1 ) * Option.get(eval e2 ));


                    
