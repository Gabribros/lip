%{
open Ast
%}

%token TRUE
%token FALSE

%token <int> CONST
%token <string> VAR

%token LPAREN
%token RPAREN
%token THEN
%token ELSE
%token NOT
%token AND
%token OR

%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token SEQ

%token IF
%token WHILE
%token DO
%token EOF

%start <cmd> prog

%nonassoc ELSE
%left SEQ 
%nonassoc WHILE IF

%left OR
%left AND
%left NOT
%left MUL
%left ADD
%left SUB 


%%

prog:
  | e = cmd; EOF { e }
;

expr:
  | var = VAR; { Var( var ) }
  | const = CONST; {  Const(const) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e = expr { Not e }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr; { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr; {Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;


cmd:  
  | SKIP; { Skip }
  | name = VAR; ASSIGN; e=expr; { Assign(name,e) }
  | e1 = cmd; SEQ; e2 = cmd; { Seq(e1,e2) }
  | IF; e1 = expr; THEN; e2 = cmd; ELSE; e3 = cmd; { If(e1, e2, e3) } 
  | WHILE; e1 = expr; DO; e2 = cmd; { While(e1,e2) }
  | LPAREN; e=cmd; RPAREN {e}
;
