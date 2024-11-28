open Zpp_interp
open OUnit

let make_i n i s =
  n >:: (fun _ -> assert_equal i (s |> run |> int_of_expr))

let make_f n f s =
  n >:: (fun _ -> assert_equal f (s |> run |> float_of_expr))

let make_s n str s =
  n >:: (fun _ -> assert_equal str (s |> run |> string_of_expr))

let make_b n b s =
  n >:: (fun _ -> assert_equal b (s |> run |> bool_of_expr))

let make_fail n s' s =
  n >:: (fun _ -> assert_raises (Failure s') (fun _ -> run s))
