%{
open Rule
%}

%token <string> CONST

%token EOF
%token START
%token DIVIDER

%start <rule> prog

%%

prog:
  | e = expr; EOF { e }
;

num:
| n = CONST { int_of_string n }
;

expr:
  | START; e1=list (num); DIVIDER; e2=list (num); { Rule(e1,e2) }
;
