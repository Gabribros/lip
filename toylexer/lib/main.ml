open Token
open StdLabels.List 
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n l = 

let rec find tupl_list el = match tupl_list with
|[]-> false
|(a,_)::_ when (a = el) -> true
|_::l1 -> find l1 el
in

let rec makeTupl list_el list num = match list with
|[]-> (list_el,num)
|b::l1 when (list_el = b) -> makeTupl list_el l1 (num+1)
|_::l1 -> makeTupl list_el l1 num
in

let rec mainCycle num list tupl = match list with
|[] -> tupl
|_ when (num = 0) -> tupl
|a::l1 when (find tupl a = false) ->  mainCycle (num-1) (l1) (tupl@[(makeTupl a list 0)])
|_::l1 -> mainCycle (num) (l1) (tupl)

in
sort  ~cmp:(fun (_,y) (_,b) -> if(y>b) then -1 else if(y<b) then 1 else 0) (mainCycle n l [])
