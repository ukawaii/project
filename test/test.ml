open Asjad
open OUnit2

let tests = "General Test" >::: [
  "Something" >:: (fun _ -> asi ());
  "Empty rule" >:: (fun _ -> let parem = (let s = tukelda "T → " in (loo_parem_pool s)) in assert_equal parem.value True);
  "Terminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → a" in (loo_parem_pool s)) in assert_equal parem.value False);
  "NonTerminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → R" in (loo_parem_pool s)) in assert_equal parem.value Unknown);
]

let _ = run_test_tt_main tests