open Asjad
open OUnit2


let tests = "General Test" >::: [
  "Something" >:: (fun _ -> First.asi ());
  "Empty rule" >:: (fun _ -> let parem = (let s = tukelda "T → " in (loo_parem_pool s)) in assert_equal parem.value True);
  "Terminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → a" in (loo_parem_pool s)) in assert_equal parem.value False);
  "NonTerminal rule" >:: (fun _ -> let parem = (let s = tukelda "T → R" in (loo_parem_pool s)) in assert_equal parem.value Unknown);

  "T → R
   T → aTc
   R →
   R → bR" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "T" true; Hashtbl.add tabel "R" true; assert_equal tabel (Nullable.leia_nullable
  "T → R
   T → aTc
   R →
   R → bR"
  ));

  "T → R
  T → aTc
  R →
  R → RbR" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "T" true; Hashtbl.add tabel "R" true; assert_equal tabel (Nullable.leia_nullable 
  "T → R
   T → aTc
   R →
   R → RbR"
   ));

  "N → AB
  N → BA
  A → a
  A → CAC
  B → b
  B → CBC
  C → a
  C → b" >:: (fun _ -> let tabel = Hashtbl.create 69 in
  Hashtbl.add tabel "N" false; Hashtbl.add tabel "A" false;Hashtbl.add tabel "B" false;Hashtbl.add tabel "C" false; assert_equal tabel (Nullable.leia_nullable
  "N → AB
   N → BA
   A → a
   A → CAC
   B → b
   B → CBC
   C → a
   C → b"
   ));


  "T → R
   T → aTc
   R →
   R → bR" >:: (fun _ -> let module Set = Set.Make(String) in 
   let str =
    "T → R
    T → aTc
    R →
    R → bR" in
   let table = First.leia_first_string_nullable str (Nullable.leia_nullable str) in let set = Set.add "a" (Set.singleton "b") in 
    assert_equal true (Set.equal set (Hashtbl.find table "T")); let set = Set.singleton "b" in assert_equal true (Set.equal set (Hashtbl.find table "R"))
   );


  "N → AB
   N → BA
   A → a
   A → CAC
   B → b
   B → CBC
   C → a
   C → b" >:: (fun _ -> let module Set = Set.Make(String) in 
   let str =
  "N → AB
   N → BA
   A → a
   A → CAC
   B → b
   B → CBC
   C → a
   C → b" in let table = First.leia_first_string_nullable str (Nullable.leia_nullable str) in let set = Set.add "a" (Set.singleton "b") in
   assert_equal true (Set.equal set (Hashtbl.find table "N"));assert_equal true (Set.equal set (Hashtbl.find table "A"));
   assert_equal true (Set.equal set (Hashtbl.find table "B"));assert_equal true (Set.equal set (Hashtbl.find table "C"));
    );

   
]

let _ = run_test_tt_main tests