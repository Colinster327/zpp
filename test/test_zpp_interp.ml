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

let tests = [
  make_i "add" 6 "3 + 3";
  make_i "sub" 1 "3 - 2";
  make_i "mult" 6 "3 * 2";
  make_i "div" 2 "4 / 2";
  make_fail "div_zero" div_zero_err "4 / 0";
  make_b "leq_true" true "3 <= 4";
  make_b "leq_false" false "4 <= 3";
  make_b "lt_true" true "3 < 4";
  make_b "lt_false" false "4 < 3";
  make_b "gt_true" true "4 > 3";
  make_b "gt_false" false "3 > 4";
  make_b "geq_true" true "4 >= 3";
  make_b "geq_false" false "3 >= 4";
  make_b "equal_int_true" true "3 = 3";
  make_b "equal_int_false" false "3 = 4";
  make_f "add_float" 5.5 "2.5 + 3.0";
  make_f "sub_float" 1.5 "3.5 - 2.0";
  make_f "mult_float" 7.5 "2.5 * 3.0";
  make_f "div_float" 2.0 "4.0 / 2.0";
  make_fail "div_float_zero" div_zero_err "4.0 / 0.0";
  make_b "leq_float_true" true "3.5 <= 4.0";
  make_b "leq_float_false" false "4.5 <= 4.0";
  make_b "lt_float_true" true "3.5 < 4.0";
  make_b "lt_float_false" false "4.5 < 4.0";
  make_b "gt_float_true" true "4.5 > 4.0";
  make_b "gt_float_false" false "3.5 > 4.0";
  make_b "geq_float_true" true "4.5 >= 4.0";
  make_b "geq_float_false" false "3.5 >= 4.0";
  make_b "equal_float_true" true "3.5 = 3.5";
  make_b "equal_float_false" false "3.5 = 4.0";
  make_s "string" "hello" "\"hello\"";
  make_b "true" true "fr";
  make_b "false" false "nah";
  make_b "not_true" false "!fr";
  make_b "not_false" true "!nah";
  make_i "neg_int" (-3) "vibecheck x = 3; -x";
  make_f "neg_float" (-3.5) "vibecheck x = 3.5; -x";
  make_fail "invalid_uop" uop_err "!3";
  make_fail "invalid_bop" bop_err "3 * nah";
]

let _ = run_test_tt_main ("zpp interpreter" >::: tests)
