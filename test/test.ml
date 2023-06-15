open Asjad.Nullable
open OUnit2

let tests = "General Test" >::: [
  "Something" >:: (fun _ -> asi ());
  "Empty rule" >:: (fun _ -> let parem = (let s = tukelda "T → " in (loo_parem_pool s)) in assert_equal parem.value True);
  "Terminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → a" in (loo_parem_pool s)) in assert_equal parem.value False);
  "NonTerminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → R" in (loo_parem_pool s)) in assert_equal parem.value Unknown);

  "T → R
  T → aTc
  R →
  R → bR" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "T" true; Hashtbl.add tabel "R" true; assert_equal tabel (leia_nullable "T → R
  T → aTc
  R →
  R → bR"));

  "T → R
  T → aTc
  R →
  R → RbR" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "T" true; Hashtbl.add tabel "R" true; assert_equal tabel (leia_nullable "T → R
  T → aTc
  R →
  R → RbR"));


  "N → AB
  N → BA
  A → a
  A → CAC
  B → b
  B → CBC
  C → a
  C → b" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "N" false; Hashtbl.add tabel "A" false;Hashtbl.add tabel "B" false;Hashtbl.add tabel "C" false; assert_equal tabel (leia_nullable "N → AB
  N → BA
  A → a
  A → CAC
  B → b
  B → CBC
  C → a
  C → b"))
]

let _ = run_test_tt_main tests