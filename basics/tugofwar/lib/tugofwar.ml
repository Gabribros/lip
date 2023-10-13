(* tokens *)
type token = A | B | X


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
             
(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
       
let toklist_of_string s = 
let rec convertList lc lt = match lc with
| 'A'::l1 ->  convertList l1 (lt@[A])
| 'B'::l1 -> convertList l1 (lt@[B])
| '='::l1 ->convertList l1 (lt@[X])
| [] -> lt
| _ ->failwith "String contains symbols outside the alphabet"
in
convertList (explode s) []

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l =
let rec checkList lt s = match lt with
| [] when (s != B )->false
| [] -> true
| A::l1 when(s = A)->checkList l1 A
| X::l1 when(s = X)->checkList l1 X
| B::l1 when(s = B)->checkList l1 B
| X::l1 when(s = A)-> checkList l1 X
| B::l1 when(s = X)->checkList l1 B
| _ -> false
in
match l with
|[] -> false
| _ -> checkList l A


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l =
let rec counter lt (a,b) = match lt with
| A::l1 -> counter l1 (a+1,0)
| B::l1 -> counter l1 (a,b+1)
| X::l1 -> counter l1 (a,0)
| [] -> (a,b) 
in
let result = counter l (0,0)
in
match result with
|(a,b) when (a>b) -> A
|(a,b) when(a=b)->X
|_-> B

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
|A->"A"
|B->"B"
|X->"="
