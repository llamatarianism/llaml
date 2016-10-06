
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | VAL
  | TYPE of (string)
  | THEN
  | STRING of (string)
  | SEMISEMI
  | SEMI
  | RPAREN
  | RBRACKET
  | PIPE
  | OPER of (string)
  | LPAREN
  | LET
  | LBRACKET
  | LAMBDA
  | INT of (int)
  | IN
  | IF
  | FLOAT of (float)
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DATA
  | COMMA
  | COLON
  | BOOL of (bool)
  | BEGIN
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.statement list)
