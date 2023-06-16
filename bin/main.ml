open Asjad

let () =

let grammer =
  
"N → AB
N → BA
A → a
A → CAC
B → b
B → CBC
C → a
C → b"

in let null =
  Nullable.leia_nullable grammer in
  print_nullable_all null;

let first =
  First.leia_first_string_nullable grammer null in 
  First.print_set_all first