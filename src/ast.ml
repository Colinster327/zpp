type uop =
  | Not
  | Neg

type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Leq
  | Lt
  | Gt
  | Geq
  | Equal
  | NEqual
  | Concat
  | Conj
  | Disj

type expr =
  | Unit
  | Int of int
  | Float of float
  | Str of string
  | Ident of string
  | True
  | False
  | Unop of uop * expr
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | Upd of string * expr * expr
  | Tny of expr * expr * expr
  | Ite of expr * expr * expr * expr
  | While of expr * expr * expr
  | Cout of expr * expr
