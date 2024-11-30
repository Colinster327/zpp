val uop_err : string
val bop_err : string
val div_zero_err : string
val tny_err : string
val ite_err : string
val cout_err : string
val while_err : string
val app_err : string
val num_arg_err : string

val run : string -> Ast.expr

val int_of_expr : Ast.expr -> int
val float_of_expr : Ast.expr -> float
val bool_of_expr : Ast.expr -> bool
val string_of_expr : Ast.expr -> string
