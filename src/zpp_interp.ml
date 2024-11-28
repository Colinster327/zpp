open Ast

type 'a env = string -> 'a

let init_env : 'a env = fun x -> failwith (x ^ " is unbound")
let lookup (rho : 'a env) (x : string) : 'a = rho x 
let upd (rho : 'a env) (x : string) (new_a : 'a) : 'a env =
  fun y -> if x = y then new_a else rho y

let uop_err = "Invalid Unary Expression"
let bop_err = "Invalid Binary Expression"
let div_zero_err = "Division By Zero"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let rec interp rho = function
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Str s -> Str s
  | Ident x -> lookup rho x
  | True -> True
  | False -> False
  | Unop (uop, e) ->
    let ie = interp rho e in
    (match uop, ie with
      | Not, True -> False
      | Not, False -> True
      | Neg, Int i -> (Int (-i))
      | Neg, Float f -> (Float (-.f))
      | _, _ -> failwith uop_err)
  | Binop (bop, e1, e2) ->
    let ie1 = interp rho e1 in
    let ie2 = interp rho e2 in
    (match bop, ie1, ie2 with
      (* Int operations *)
      | Add, Int x, Int y -> Int (x + y)
      | Sub, Int x, Int y -> Int (x - y)
      | Mult, Int x, Int y -> Int (x * y)
      | Div, Int x, Int y -> 
        if y <> 0 then Int (x / y) else failwith div_zero_err 
      | Leq, Int x, Int y ->
        if x <= y then True else False
      | Lt, Int x, Int y ->
        if x < y then True else False
      | Gt, Int x, Int y ->
        if x > y then True else False
      | Geq, Int x, Int y ->
        if x >= y then True else False
      | Equal, Int x, Int y ->
        if x = y then True else False
      (* Float operations *)
      | Add, Float x, Float y -> Float (x +. y)
      | Sub, Float x, Float y -> Float (x -. y)
      | Mult, Float x, Float y -> Float (x *. y)
      | Div, Float x, Float y -> 
        if y <> 0. then Float (x /. y) else failwith div_zero_err 
      | Leq, Float x, Float y ->
        if x <= y then True else False
      | Lt, Float x, Float y ->
        if x < y then True else False
      | Gt, Float x, Float y ->
        if x > y then True else False
      | Geq, Float x, Float y ->
        if x >= y then True else False
      | Equal, Float x, Float y ->
        if x = y then True else False
      (* Bool operations *)
      | Equal, True, True -> True
      | Equal, False, False -> True
      | Equal, True, False -> False
      | Equal, False, True -> False
      | _ -> failwith bop_err)
  | Let (x, e1, e2) ->
    let ie1 = interp rho e1 in
    let new_env = upd rho x ie1 in
    interp new_env e2
  | Ite (e1, e2, e3) ->
    let ie1 = interp rho e1 in
    (match ie1 with
      | True -> interp rho e2
      | False -> interp rho e3
      | _ -> failwith "Invalid If-Then-Else Expression")
  | Cout e ->
    let ie = interp rho e in
    (match ie with
      | Str s -> print_string s; Unit
      | _ -> failwith "Invalid Output Expression")

  let run s =
    s |> parse |> interp init_env

  let int_of_expr = function
    | Int x -> x
    | _ -> failwith "Not type Int"

  let float_of_expr = function
    | Float x -> x
    | _ -> failwith "Not type Float"

  let bool_of_expr = function
    | True -> true
    | False -> false
    | _ -> failwith "Not type Bool"

  let string_of_expr = function
    | Str s -> s
    | _ -> failwith "Not type String"
