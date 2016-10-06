type name = string [@@deriving show]

                   
type ty =
  | TAtom    of name
  | TGeneric of name
  | TParam   of (name * ty list)
  | TArrow   of (ty * ty)
  [@@deriving show]

                  
type ast =
  | Int        of int
  | Float      of float
  | Bool       of bool
  | String     of string
  | Lambda     of (name * ast) (* fun NAME -> AST e.g. fun x -> x + 1 *)
  | Var        of name
  | Call       of (ast * ast)
  | List       of ast list
  | Tuple      of ast list
  | Sequential of ast list
  | Let        of ((name * ast) list * ast)
  | If         of (ast * ast * ast)
  [@@deriving show]

  
type statement =
  | Type     of (name * ty)
  | VarDecl  of (name * ast) list
  | TypeDecl of (name * ty)
  [@@deriving show]
