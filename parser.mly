%{
  open Ast
%}

%token <int>    INT
%token <bool>   BOOL
%token <float>  FLOAT
%token <string> VAR
%token <string> OPER
%token <string> TYPE
%token <string> STRING
%token LET EQ IN
%token IF THEN ELSE
%token VAL DATA
%token LAMBDA ARROW
%token LPAREN RPAREN BEGIN END
%token LBRACKET RBRACKET
%token PIPE
%token SEMISEMI SEMI COMMA COLON DOT
%token EOF

%left OPER
%right ARROW

%start <Ast.statement list> main

%%

main:
  statement+ EOF
    { $1 }

simple_expr:
  | INT
    { Int $1 }
  | FLOAT
    { Float $1 }
  | BOOL
    { Bool $1 }
  | STRING
    { String $1 }
  | VAR
    { Var $1 }
  | LPAREN expr RPAREN
    { $2 }
  | BEGIN expr END
    { $2 }
  | LBRACKET separated_list(SEMI, avg_expr) RBRACKET
    { List $2 }

avg_expr:
  | simple_expr { $1 }
  | call        { $1 }
  
expr:
  | avg_expr
    { $1 }
  | IF c = avg_expr THEN t = expr ELSE e = expr
    { If (c, t, e) }
  | LET b = bindings IN e = expr
    { Let (b, e) }
  | LET PIPE b = bindings IN e = expr
    { Let (b, e) }
  | tuple
    { Tuple $1 }
  | seq
    { Sequential $1 }
  | func
    { $1 }

statement:
  | LET b = bindings
    { VarDecl b }
  | VAL; v = VAR; COLON; t = ty
    { Type (v, t) }
  | DATA; t = TYPE; EQ; u = ty
    { TypeDecl (t, u) }

simple_ty:
  | t = TYPE
    { TAtom t }
  | v = VAR
    { TGeneric v }
  | LPAREN ty RPAREN
    { $2 }

ty:
  | simple_ty
    { $1 }
  | t = TYPE; ts = simple_ty+
    { TParam (t, ts) }
  | t = ty; ARROW; u = ty
    { TArrow (t, u) }

tuple:
  rev_tuple { List.rev $1 }

rev_tuple:
  | rev_tuple COMMA avg_expr
    { $3 :: $1 }
  | avg_expr COMMA avg_expr
    { [$3; $1] }

seq:
  rev_seq { List.rev $1 };

rev_seq:
  | rev_seq SEMI avg_expr
    { $3 :: $1 }
  | avg_expr SEMI avg_expr
    { [$3; $1] }


call:
  | prefix_call { $1 }
  | infix_call  { $1 }

prefix_call:
  | prefix_call simple_expr
    { Call ($1, $2) }
  | simple_expr simple_expr
    { Call ($1, $2) }

infix_call:
  e = avg_expr; o = OPER; f = avg_expr
    { Call (Call (Var ("(" ^ o ^ ")"), e), f) }


func:
  LAMBDA; vs = VAR+; ARROW; e = expr
    { List.fold_right (fun x acc -> Lambda (x, acc)) vs e }


bindings:
  b = separated_list(PIPE, binding) { b }

binding:
  | v = VAR EQ e = expr
    { (v, e) }
  | f = VAR; vs = VAR+; EQ; e = expr
    { let e = List.fold_right (fun x acc -> Lambda (x, acc)) vs e in
      (f, e)
    }