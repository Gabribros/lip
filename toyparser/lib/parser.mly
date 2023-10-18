%{
open Ast
%}

%token <string> CONST
%token HEX
%token PLUS
%token MINUS
%token DIVISION
%token MULTIPLY
%token LPAREN
%token RPAREN
%token EOF

%left MINUS PLUS
%left DIVISION MULTIPLY

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | MINUS; e1 = expr { Negate(e1) }
  | HEX; e1 = expr { Hex(e1) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2)  }
  | e1 = expr; MINUS; e2 = expr { Minus(e1,e2) }
  | e1 = expr; DIVISION; e2 = expr { Division(e1,e2)  }
  | e1 = expr; MULTIPLY; e2 = expr { Multiply(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
