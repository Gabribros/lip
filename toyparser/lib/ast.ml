type ast =
    Const of int 
  | Negate of ast
  | Add of ast * ast  
  | Minus of ast  * ast 
  | Division of ast * ast 
  | Multiply of ast * ast 

