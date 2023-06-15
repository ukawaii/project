
type t = char [@@deriving ord, show]

let testing_1 () = 
  let s =  "T → R
  T → aT c
  R →
  R → bR" in

print_endline (String.concat"\n" (String.split_on_char '\n' s));;

let testing_2 s =
let regString = "→" in 
let reg = Str.regexp regString in

print_endline (String.concat "--" (Str.split reg s));;

let is_lower_case c = let reg = Str.regexp "[a-z]" in Str.string_match reg c 0  ;;

let tukelda s =
  let reg = Str.regexp "→" in (Str.split reg s);;


type value_atom = True | False | Unknown [@@deriving show]

type parem ={
  tervik : string;
  value : value_atom
}[@@deriving show]

type terminal = {
  vasak_pool : string;
  parem_pool : parem list;
}

let loo_parem_pool s = let l = tukelda s in
let parem_pool = List.nth l 1 in let parem_pool = String.trim parem_pool in if parem_pool = "" then {tervik = parem_pool; value = True} else
if is_lower_case parem_pool then {tervik = parem_pool; value = False} else {tervik = parem_pool; value = Unknown};;

let asi () = let par = (loo_parem_pool "T → ") in
print_endline (par.tervik); print_endline (show_value_atom par.value)