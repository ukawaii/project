open Asjad.Nullable

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

in let x =
leia_nullable grammer in
print_nullable_all x