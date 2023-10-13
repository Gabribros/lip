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


let lang3 l =
let rec check_else lo = match lo with
| '0'::[] -> true
| '1'::l1 -> check_else l1
| '0'::l1 -> check_else l1
| _ -> false
in

match l with
|[] -> true
|'0'::l1 -> check_else l1
| _ ->false


let lang4 l =
let rec belongs4 lo c = match lo with
| [] when (c = 2)-> true
| '1'::_ when (c = 2)-> false
| '1'::l1 -> belongs4 l1 (c+1)
| '0'::l1 -> belongs4 l1 c
| _ -> false
in
belongs4 l 0

let lang5 l = 
let rec belongs5 lo o z = match lo with
| '0'::l1 when( (o = 0 || o = 2) && z<2) -> belongs5 l1 0 (z+1)
| '1'::l1 when ( (z = 0 || z = 2) && o<2) -> belongs5 l1 (o+1) 0
| '0'::l1 when( o = 0 && z=2) -> belongs5 l1 0 1
| '1'::l1 when ( z = 0 && o=2) -> belongs5 l1 1 0
| [] when ((z = 0 || z=2 ) && (o =0 || o = 2)) -> true
|_ -> false
in
match l with
|[] -> false
| _ -> belongs5 l 0 0

    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
 

