open Ast

type 'a env = ((string * 'a) ref) list

let init_env : 'a env = []

let rec lookup (rho : 'a env) (x : string) : 'a =
  match rho with
  | [] -> failwith (x ^ " is unbound")
  | h :: t ->
    let (x', a_val) = !h in
    if x' = x then a_val else lookup t x

let add_to_env (rho : 'a env) (x : string) (new_a : 'a) : 'a env =
  let new_env_val = ref (x, new_a) in
  new_env_val :: rho

let rec upd_env (rho : 'a env) (x : string) (upd_a : 'a) : unit =
  match rho with
  | [] -> failwith (x ^ " is unbound")
  | h :: t ->
    let (x', _) = !h in
    if x' = x then h := (x', upd_a) else upd_env t x upd_a

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
  | Ident x ->
    let e = lookup rho x in
    interp rho e
  | True -> True
  | False -> False
  | Unop (uop, e) ->
    let ie = interp rho e in
    (match uop, ie with
      (* Bool operations *)
      | Not, True -> False
      | Not, False -> True
      (* Int operations *)
      | Neg, Int i -> (Int (-i))
      (* Float operations *)
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
      | NEqual, Int x, Int y ->
        if x <> y then True else False
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
      | NEqual, Float x, Float y ->
        if x <> y then True else False
      (* Bool operations *)
      | Equal, True, True -> True
      | Equal, False, False -> True
      | Equal, True, False -> False
      | Equal, False, True -> False
      | NEqual, True, True -> False 
      | NEqual, False, False -> False 
      | NEqual, True, False -> True 
      | NEqual, False, True -> True
      | Conj, True, True -> True
      | Conj, True, False -> False
      | Conj, False, True -> False
      | Conj, False, False -> False
      | Disj, True, True -> True
      | Disj, True, False -> True 
      | Disj, False, True -> True 
      | Disj, False, False -> False
      (* String operations *)
      | Concat, Str x, Str y -> Str (x ^ y)
      | Concat, Int x, Str y -> Str (string_of_int x ^ y)
      | Concat, Float x, Str y -> Str (string_of_float x ^ y)
      | Concat, Str x, Int y -> Str (x ^ string_of_int y)
      | Concat, Str x, Float y -> Str (x ^ string_of_float y)
      | Leq, Str x, Str y ->
        if x <= y then True else False
      | Lt, Str x, Str y ->
        if x < y then True else False
      | Gt, Str x, Str y ->
        if x > y then True else False
      | Geq, Str x, Str y ->
        if x >= y then True else False
      | Equal, Str x, Str y ->
        if x = y then True else False
      | NEqual, Str x, Str y ->
        if x <> y then True else False
      | _ -> failwith bop_err)
  | Let (x, e1, e2) ->
    let ie1 = interp rho e1 in
    let new_env = add_to_env rho x ie1 in
    interp new_env e2
  | Upd (x, e1, e2) ->
    let ie1 = interp rho e1 in
    upd_env rho x ie1;
    interp rho e2
  | Tny (e1, e2, e3) ->
    let ie1 = interp rho e1 in
    (match ie1 with
      | True -> interp rho e2
      | False -> interp rho e3
      | _ -> failwith "Invalid If-Then-Else Expression")
  | Ite (e1, e2, e3, e4) ->
    let ie1 = interp rho e1 in
    (match ie1 with
      | True ->
        let _ = interp rho e2 in
        interp rho e4
      | False ->
        let _ = interp rho e3 in
        interp rho e4
      | _ -> failwith "Invalid If-Then-Else Expression")
  | Cout (e1, e2) ->
    let ie1 = interp rho e1 in
    (match ie1 with
      | Str s -> print_string s; interp rho e2
      | _ -> failwith "Invalid Output Expression")
  | While (e1, e2, e3) ->
    let ie1 = interp rho e1 in
    (match ie1 with
      | True -> 
        let _ = interp rho e2 in
        interp rho (While (e1, e2, e3))
      | False -> interp rho e3
      | _ -> failwith "Invalid While Loop Expression")

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
