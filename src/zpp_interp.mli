type 'a env

val uop_err : string
val bop_err : string
val div_zero_err : string

val parse : string -> Ast.expr
val interp : Ast.expr env -> Ast.expr -> Ast.expr
val run : string -> Ast.expr

val int_of_expr : Ast.expr -> int
val float_of_expr : Ast.expr -> float
val bool_of_expr : Ast.expr -> bool
val string_of_expr : Ast.expr -> string
