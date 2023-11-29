open Ast

let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | If (e0, e1, e2) ->
      "If(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ ","
      ^ string_of_expr e2 ^ ")"
  | Not e -> "Not(" ^ string_of_expr e ^ ")"
  | And (e1, e2) -> "And(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "Or(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Succ e -> "Succ(" ^ string_of_expr e ^ ")"
  | Pred e -> "Pred(" ^ string_of_expr e ^ ")"
  | IsZero e -> "IsZero(" ^ string_of_expr e ^ ")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec is_nv = function Zero -> true | Succ e -> is_nv e | _ -> false

let rec trace1 = function
  | If (True, e1, _) -> e1
  | If (False, _, e2) -> e2
  | If (e0, e1, e2) ->
      let e0' = trace1 e0 in
      If (e0', e1, e2)
  | Not True -> False
  | Not False -> True
  | Not e -> Not (trace1 e)
  | And (True, e2) -> e2
  | And (False, _) -> False
  | And (e1, e2) -> And (trace1 e1, e2)
  | Or (False, e2) -> e2
  | Or (True, _) -> True
  | Or (e1, e2) -> Or (trace1 e1, e2)
  | Succ e -> Succ (trace1 e)
  | Pred Zero -> raise NoRuleApplies
  | Pred (Succ nv) when is_nv nv -> nv
  | Pred e -> Pred (trace1 e)
  | IsZero Zero -> True
  | IsZero (Succ nv) when is_nv nv -> False
  | IsZero e -> IsZero (trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e =
  try
    let e' = trace1 e in
    e :: trace e'
  with NoRuleApplies -> [ e ]

type exprval = Bool of bool | Nat of int

let string_of_val = function
  | Bool b -> string_of_bool b
  | Nat n -> string_of_int n

exception InvalidArg of string

let rec eval e =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If (e0, e1, e2) -> if evalbool e e0 then eval e1 else eval e2
  | Not e' -> Bool (not (evalbool e e'))
  | And (e1, e2) -> Bool (evalbool e e1 && evalbool e e2)
  | Or (e1, e2) -> Bool (evalbool e e1 || evalbool e e2)
  | Succ e' -> Nat (evalnat e e' + 1)
  | Pred e' ->
      let n = evalnat e e' in
      if n > 0 then Nat (n - 1) else raise (InvalidArg "Negative nat")
  | IsZero e' -> Bool (evalnat e e' = 0)

and evalbool e arg =
  match eval arg with
  | Bool b -> b
  | _ ->
      raise
        (InvalidArg
           ("In " ^ string_of_expr e ^ ", " ^ string_of_expr arg
          ^ " has type nat but a value was expected of type bool."))

and evalnat e arg =
  match eval arg with
  | Nat n -> n
  | _ ->
      raise
        (InvalidArg
           ("In " ^ string_of_expr e ^ ", " ^ string_of_expr arg
          ^ " has type bool but a value was expected of type nat."))
