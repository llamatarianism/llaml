{
  open Parser

  exception Syntax_error of string
}

let digit = ['0'-'9']

let oper_start_syms = ['+' '-' '*' '/' '&' '>' '<' '$' '@' '^' '%'] 
let oper = "==" | oper_start_syms ( oper_start_syms | ['=' '.' '|' '\'' '\\'] )*
let ident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'' '-']* | '(' oper ')'
let typen = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

let newl = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+ | newl

let int = '-'? digit+
let float = int '.' digit+

rule string buf = parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' '\\'     { Buffer.add_char buf '\\'; string buf lexbuf }
  | '\\' 'b'      { Buffer.add_char buf '\b'; string buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof
    { raise (Syntax_error "Unterminated string literal") }

and read = parse
  | white   { read lexbuf }
  | int     { INT (lexbuf |> Lexing.lexeme |> int_of_string) }
  | float   { FLOAT (lexbuf |> Lexing.lexeme |> float_of_string) }
  | "True"  { BOOL true }
  | "False" { BOOL false }
  | "let"   { LET }
  | "in"    { IN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "begin" { BEGIN }
  | "end"   { END }
  | "val"   { VAL }
  | "data"  { DATA }
  | typen   { TYPE (Lexing.lexeme lexbuf) }
  | ident   { VAR (Lexing.lexeme lexbuf) }
  | '\\'    { LAMBDA }
  | '"'     { string (Buffer.create 15) lexbuf }
  | '#'     { comment lexbuf }
  | "->"    { ARROW }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '['     { LBRACKET }
  | ']'     { RBRACKET }
  | '|'     { PIPE }
  | ";;"    { SEMISEMI }
  | ';'     { SEMI }
  | ':'     { COLON }
  | ','     { COMMA }
  | '='     { EQ }
  | '.'     { DOT }
  | oper    { OPER (Lexing.lexeme lexbuf) }
  | _       { raise (Syntax_error ("Unexpected char " ^ (Lexing.lexeme lexbuf))) }
  | eof     { EOF }

and comment = parse
  | '\n' { read lexbuf }
  | _    { comment lexbuf }