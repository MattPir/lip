%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token NOT
%token AND
%token OR
%token ZERO
%token SUCC
%token PRED
%token IS_ZERO

%left OR
%left NOT AND

%left SUCC PRED IS_ZERO


%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e0 = expr; AND; e1 = expr; { And(e0, e1) } 
  | e0 = expr; OR; e1 = expr; { Or(e0, e1) }
  | NOT; e = expr; { Not(e) }
  | LPAREN; e=expr; RPAREN {e}
  | ZERO { Zero }
  | SUCC; e = expr; { Succ(e) }
  | PRED; e = expr; { Pred(e) }
  | IS_ZERO; e = expr; { IsZero(e) }
;

