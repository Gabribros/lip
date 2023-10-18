type ast =
    Const of int 
  | Hex of ast
  | Negate of ast
  | Add of ast * ast  
  | Minus of ast  * ast 
  | Division of ast * ast 
  | Multiply of ast * ast 

