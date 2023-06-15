open Asjad
open OUnit2

let tests = "General Test" >::: [
  "Something" >:: (fun _ -> asi ());
  "Empty rule" >:: (fun _ -> let parem = (loo_parem_pool "T → ") in assert_equal parem.value True);
  "Terminal rule" >:: (fun _ -> let parem = (loo_parem_pool "T → a") in assert_equal parem.value False);
  "NonTerminal rule" >:: (fun _ -> let parem = (loo_parem_pool "T → R") in assert_equal parem.value Unknown);
]

let _ = run_test_tt_main tests