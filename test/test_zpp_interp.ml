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

let ex1 = "vibecheck x = 5; lowkey [x < 7] {6 * 4} cap {x / 5}"
let ex2 = "vibecheck y = 10; lowkey [y > 5] {y - 3} cap {y + 3}"
let ex3 = "vibecheck a = 2; vibecheck b = 3; 4 + a * b"
let ex4 = "vibecheck x = 4; vibecheck y = 2; lowkey [x > y] {x / y} cap {y / x}"
let ex5 = "vibecheck x = 3; vibecheck y = 4; lowkey [x == y] {x + y} cap {x - y}"
let ex6 = "vibecheck x = 5.5; vibecheck y = 10.0; lowkey [x < y] {y - x} cap {x - y}"
let ex7 = "vibecheck x = 11; x <= 6; x"
let ex8 = "vibecheck x = 0; x <= x + 5; x"
let ex9 = "vibecheck x = 1; 
       vibecheck y = 2;
       x <= y + 2;
       y <= x + 2;
       x + y"
let ex10 = "vibecheck x = 60; vibecheck y = \"hello\"; y ^ x"
let ex11 = "vibecheck x = 0; grind [x < 6] {x <= x + 1;} x"
let ex12 = "fr || fr && nah"
let ex13 = "vibecheck fib = [n] => {lowkey [n < 2] { n } cap {fib [n - 1] + fib [n - 2]}}; fib [5]"
let ex14 = "vibecheck add = [x, y] => {x + y}; add [5, 5]"

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
  make_b "equal_int_true" true "3 == 3";
  make_b "equal_int_false" false "3 == 4";
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
  make_b "equal_float_true" true "3.5 == 3.5";
  make_b "equal_float_false" false "3.5 == 4.0";
  make_b "not_equal_int_true" true "3 != 4";
  make_b "not_equal_int_false" false "3 != 3";
  make_b "not_equal_float_true" true "3.5 != 4.0";
  make_b "not_equal_float_false" false "3.5 != 3.5";
  make_b "conj" true "fr && fr";
  make_b "disj" false "nah || nah";
  make_fail "invalid conj" bop_err "5 && nah";
  make_s "string" "hello" "\"hello\"";
  make_s "str-str concat" "hello world" "\"hello\" ^ \" world\"";
  make_s "int-str concat" "5test" "5 ^ \"test\"";
  make_s "str-float concat" "test5.1" "\"test\" ^ 5.1";
  make_fail "int-int concat" bop_err "5 ^ 10";
  make_b "true" true "fr";
  make_b "false" false "nah";
  make_b "not_true" false "!fr";
  make_b "not_false" true "!nah";
  make_i "neg_int" (-3) "vibecheck x = 3; -x";
  make_f "neg_float" (-3.5) "vibecheck x = 3.5; -x";
  make_fail "invalid_uop" uop_err "!3";
  make_fail "invalid_bop" bop_err "3 * nah";
  make_i "complex_expr1" 24 ex1;
  make_i "complex_expr2" 7 ex2;
  make_i "complex_expr3" 10 ex3;
  make_i "complex_expr4" 2 ex4;
  make_i "complex_expr5" (-1) ex5;
  make_f "complex_expr6" 4.5 ex6;
  make_i "assign" 6 ex7;
  make_i "increment" 5 ex8;
  make_i "complex assign" 10 ex9;
  make_s "complex concat" "hello60" ex10;
  make_i "while loop" 6 ex11;
  make_b "complex bool op" true ex12;
  make_i "fibonacci seq" 5 ex13;
  make_i "multi-var func" 10 ex14;
  make_b "str_leq_true" true "\"apple\" <= \"banana\"";
  make_b "str_leq_false" false "\"banana\" <= \"apple\"";
  make_b "str_lt_true" true "\"apple\" < \"banana\"";
  make_b "str_lt_false" false "\"banana\" < \"apple\"";
  make_b "str_geq_true" true "\"banana\" >= \"apple\"";
  make_b "str_geq_false" false "\"apple\" >= \"banana\"";
  make_b "str_gt_true" true "\"banana\" > \"apple\"";
  make_b "str_gt_false" false "\"apple\" > \"banana\"";
  make_b "str_equal_true" true "\"hello\" == \"hello\"";
  make_b "str_equal_false" false "\"hello\" == \"world\"";
  make_b "str_not_equal_true" true "\"hello\" != \"world\"";
  make_b "str_not_equal_false" false "\"hello\" != \"hello\"";
  make_b "int_not_equal_true" true "3 != 4";
  make_b "int_not_equal_false" false "3 != 3";
  make_b "float_not_equal_true" true "3.5 != 4.0";
  make_b "float_not_equal_false" false "3.5 != 3.5";
]

let _ = run_test_tt_main ("zpp interpreter" >::: tests)
