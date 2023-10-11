let rec lang1 l = 
match l with
| a::[] when (a = '0' || a = '1') -> true
| [] -> false;
| '0'::l1 -> lang1 l1
| '1'::l1 -> lang1 l1
| _  -> false

let lang2 l =
let rec check_Ones lo = match lo with
| [] -> true
| '1'::l1 -> check_Ones l1
| _ -> false
in

match l with
|[] -> true
|'0'::l1 -> check_Ones l1
|'1'::l1 -> check_Ones l1
|_ ->false


let lang3 _ = false

let lang4 _ = false

let lang5 _ = false
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
 

