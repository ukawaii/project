
(*Abivahendid testimaks kas on väiketähed e terminalid*)
let is_lower_case_string c = let reg = Str.regexp "[a-z]" in Str.string_match reg c 0  ;;
let is_lower_case_char c = let c' = Char.lowercase_ascii c in c = c' ;;

(*Eraldab vasaku ja parema poole grammatika reeglil*)
let tukelda s =
  let s = String.trim s in
  let reg = Str.regexp "→" in (Str.split reg s);;

let gram_to_list s = String.split_on_char '\n' s;;


type value_atom = True | False | Unknown [@@deriving show]

type parem ={
  tervik : string;
  mutable value : value_atom
}

type terminal = {
  vasak_pool : string;
  parem_pool : parem list;
}

let table = Hashtbl.create 69;;

let loo_parem_pool l =
  let l' = 
  if List.length l = 1 then List.append l ["ε"]
  else l
  in
  let parem_pool = List.nth l' 1 in let parem_pool = String.trim parem_pool in if parem_pool = "ε" then {tervik = parem_pool; value = True} else
  if is_lower_case_string parem_pool then {tervik = parem_pool; value = False} else {tervik = parem_pool; value = Unknown};;

let loo_vasak_pool l = let vasak_pool = List.nth l 0 in let vasak_pool = String.trim vasak_pool in
  match Hashtbl.find_opt table vasak_pool with
  | None -> Hashtbl.add table vasak_pool [loo_parem_pool l]
  | _ -> Hashtbl.replace table vasak_pool ([loo_parem_pool l] @ (Hashtbl.find table vasak_pool))
  ;;

let loo_pooled l = List.iter (fun x -> loo_vasak_pool (tukelda x)) l
;;

(*Abivahendid Grammatika ja nullable lahendite nägemiseks*)
let print_terminal term table = let parem = Hashtbl.find table term in
List.iter (fun x -> print_endline("Terminal: " ^ term ^ " ParemPool :" ^ x.tervik ^ " Väärtus :" ^ (show_value_atom x.value))) parem ;;

let print_terminal_all table = Hashtbl.iter (fun x _ -> print_terminal x table) table;;

let print_nullable term table =print_endline ("Terminal: " ^ term ^ " Nullable: " ^  string_of_bool(Hashtbl.find table term));;

let print_nullable_all table = Hashtbl.iter (fun x _ -> print_nullable x table) table;;

let test_string_1 = 
  "T → R
   T → aTc
   R → ε
   R → bR";;
  
let test_string_2 = 
 "A → BAa
  A →
  B → bBc
  B → AA";;

let test_string_3 =
    "N → AB
    N → BA
    A → a
    A → CAC
    B → b
    B → CBC
    C → a
    C → b";;

(* lõputu*)
let test_string_4 =
  "S → A
    A → S";;
 module Nullable =
 struct

let nullable  = Hashtbl.create 69 ;;

let contains_unknown l = let mappo = List.map (fun x -> x.value = Unknown) l in List.mem true mappo ;;

(* Kui leidub 1 tõeväärtus siis on tõene, sest nullable(N) = a v b v c *)
let contains_true l = let mappo = List.map (fun x -> x.value = True) l in List.mem true mappo ;;
 

(* Üritab väärtustada Mitteterminali*)
let val_unknown parem =
let c = List.init (String.length parem.tervik) (String.get parem.tervik) in
let c = List.map (fun x -> Char.escaped x) c in
let vaartused = List.map (fun x -> Hashtbl.find_opt nullable x) c in
let _contain_none = List.mem None vaartused in
let _contain_false = List.mem (Some false) vaartused in
if _contain_false then parem.value <- False
else
begin if _contain_none then ()
else parem.value <- True
end


let try_val_unknown parem_list = List.iter (fun x -> if x.value = Unknown then val_unknown x else ()) parem_list
;;

(*Väärtustab Mitteterminalid kui neil ei ole Paremal poolel teadmata väärtusega Mitteterminali, muidu saadab edasi*)
let try_eval term parem_list = let value = contains_true parem_list in
let unknown = contains_unknown parem_list in 
  match value, unknown with
  | true, _ -> Hashtbl.replace nullable term true
  | false, false -> Hashtbl.replace nullable term false
  | false, true -> try_val_unknown parem_list
 ;;

(*Jooksutab seni kuni igal Mitteterminalil on nullable väärtus*)
let rec work_horse () =
  if Hashtbl.length nullable = Hashtbl.length table then nullable
  else 
    (Hashtbl.iter (fun x y -> try_eval x y) table; work_horse ());;

let leia_nullable str =
  let list = gram_to_list str in loo_pooled list;
  work_horse();;
 
end

module First = 
struct

let first = Hashtbl.create 69;;
module Set = Set.Make(String);;

(*Abivahendid Set nägemiseks*)
let print_set set = String.concat ", " (Set.elements set) ;; 
let print_set_all hshtbl = Hashtbl.iter (fun x y -> print_endline("Terminal: " ^ x ^ " First: " ^ print_set y)) hshtbl ;;
  
(*Võtab parema poole ja loob sellest Set'i arvestades nullable väärtusi*)
let parem_to_set parem x nullable = let char_list = List.init (String.length parem.tervik) (String.get parem.tervik) in
let rec list_to_set array =
  match array with
  | [] -> Set.empty
  | h :: t ->
  begin
    match h with
      | h when h = '\181' -> Set.empty (*ma ei tea kus see tekib*)
      | h when (h = "ε".[0]) -> Set.empty
      | h when (is_lower_case_char h) -> Set.singleton (Char.escaped h)
      | h -> if Hashtbl.find nullable (Char.escaped h) then
        Set.union (Hashtbl.find first (Char.escaped h)) (list_to_set t)
      else Hashtbl.find first (Char.escaped h)
  end
in
Hashtbl.replace first x (Set.union (Hashtbl.find first x) (list_to_set char_list));;

let step_2 x y nullable=
List.iter (fun par -> parem_to_set par x nullable) y;;

let step_1 nullable =
Hashtbl.iter (fun x y -> step_2 x y nullable) table;
;;

(*abifunksioon võrdlemaks Hashmappe sest tavaline = ei ole korrektne*)
let compareHashMaps map1 map2 =
  let vastus = ref true in
  let find_the_truth s =
    vastus := !vastus && (Set.equal (Hashtbl.find map1 s) (Hashtbl.find map2 s)) (*My dear god, kaua mul läks aega, enne kui sain aru et peab Set.equal kasutama*)
  in
  Hashtbl.iter (fun x _ -> find_the_truth x) table;
  !vastus
;;

(*Leiab first kui anda sisendiks grammatika*)
let leia_first_string str =
  let nullable = Nullable.leia_nullable str in
  (* Igal terminalil on nüüd tühi Set*)
  Hashtbl.iter (fun x _ -> Hashtbl.replace first x Set.empty) nullable; 
  let tv = ref false in
  while not !tv do
    let copy = Hashtbl.copy first in
    step_1 nullable;
    tv := compareHashMaps copy first ;
  done;
  first
;;

(*Leiab first kui anda sisendiks grammatika ja nullable lahendi*)
  let leia_first_string_nullable  _str nullable =
  Hashtbl.iter (fun x _ -> Hashtbl.replace first x Set.empty) nullable;
  let tv = ref false in
  while not !tv do
    let copy = Hashtbl.copy first in
    step_1 nullable;
    tv := compareHashMaps copy first ;
  done;
  first
;;

let asi () =
  ignore (leia_first_string test_string_2)
;;
end
