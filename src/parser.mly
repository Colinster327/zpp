%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token UNIT
%token EXCLAM
%token TRUE FALSE
%token LEQ
%token LT
%token GT
%token GEQ
%token PLUS
%token MINUS
%token TIMES
%token SLASH
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token RARROW
%token COUT
%token LET 
%token EQUAL
%token DEQUAL NEQUAL
%token IF 
%token ELSE
%token SEMI
%token COMMA
%token CARET
%token WHILE
%token CONJ
%token DISJ
%token EOF

%nonassoc EQUAL

%left DISJ
%left CONJ
%left LEQ LT GT GEQ DEQUAL CARET
%left PLUS MINUS
%left TIMES SLASH

%start<Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { Str s }
  | x = ID { Ident x }
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | LPAREN; e = expr RPAREN { e }
  | EXCLAM; e = expr { Unop (Not, e) }
  | MINUS; e = expr { Unop (Neg, e) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; SLASH; e2 = expr { Binop (Div, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; LT; e2 = expr { Binop (Lt, e1, e2) }
  | e1 = expr; GT; e2 = expr { Binop (Gt, e1, e2) }
  | e1 = expr; GEQ; e2 = expr { Binop (Geq, e1, e2) }
  | e1 = expr; DEQUAL; e2 = expr { Binop (Equal, e1, e2) }
  | e1 = expr; NEQUAL; e2 = expr { Binop (NEqual, e1, e2) }
  | e1 = expr; CARET; e2 = expr { Binop (Concat, e1, e2) }
  | e1 = expr; CONJ; e2 = expr { Binop (Conj, e1, e2) }
  | e1 = expr; DISJ; e2 = expr { Binop (Disj, e1, e2) }
  | LET; x = ID; EQUAL; e1 = expr; SEMI; e2 = expr { Let (x, e1, e2) }
  | LET; x = ID; EQUAL; e1 = expr; SEMI { Let (x, e1, Unit) }
  | x = ID; LEQ; e1 = expr; SEMI; e2 = expr { Upd (x, e1, e2) }
  | x = ID; LEQ; e1 = expr; SEMI { Upd (x, e1, Unit) }
  | LBRACE; params = id_list; RBRACE; RARROW; LBRACK; e = expr; RBRACK { Fun (params, e) }
  | e = expr; LBRACE; args = expr_list; RBRACE { FApp (e, args) }
  | IF; e1 = expr; LBRACK; e2 = expr; RBRACK; ELSE; LBRACK; e3 = expr; RBRACK; e4 = expr { Ite (e1, e2, e3, e4) }
  | IF; e1 = expr; LBRACK; e2 = expr; RBRACK; ELSE; LBRACK; e3 = expr; RBRACK { Tny (e1, e2, e3) }
  | IF; e1 = expr; LBRACK; e2 = expr; RBRACK; e3 = expr { Ite (e1, e2, Unit, e3) }
  | WHILE; e1 = expr; LBRACK; e2 = expr; RBRACK; e3 = expr { While (e1, e2, e3) }
  | WHILE; e1 = expr; LBRACK; e2 = expr; RBRACK { While (e1, e2, Unit) }
  | COUT; e1 = expr; SEMI; e2 = expr { Cout (e1, e2) }
  | COUT; e1 = expr; SEMI { Cout (e1, Unit) }

id_list:
  | x = ID { [x] }
  | x = ID; COMMA; xs = id_list { x :: xs }

expr_list:
  | e = expr { [e] }
  | e = expr; COMMA; es = expr_list { e :: es }
