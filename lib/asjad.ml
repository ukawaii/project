let is_lower_case c = let reg = Str.regexp "[a-z]" in Str.string_match reg c 0  ;;

let tukelda s =
  let reg = Str.regexp "→" in (Str.split reg s);;

let gram_to_list s = String.split_on_char '\n' s;;


type value_atom = True | False | Unknown [@@deriving show]

type parem ={
  tervik : string;
  value : value_atom
}[@@deriving show]

type terminal = {
  vasak_pool : string;
  parem_pool : parem list;
}

let hash = Hashtbl.create 69;;
(* let nullable = Hashtbl.create 69 ;;
 *)
let loo_parem_pool l =
  let l' = 
  if List.length l = 1 then List.append l ["ε"]
  else l
  in
  let parem_pool = List.nth l' 1 in let parem_pool = String.trim parem_pool in if parem_pool = "" then {tervik = parem_pool; value = True} else
  if is_lower_case parem_pool then {tervik = parem_pool; value = False} else {tervik = parem_pool; value = Unknown};;

let loo_vasak_pool l = let vasak_pool = List.nth l 0 in let vasak_pool = String.trim vasak_pool in
match Hashtbl.find_opt hash vasak_pool with
| None -> Hashtbl.add hash vasak_pool [loo_parem_pool l]
| _ -> Hashtbl.add hash vasak_pool ([loo_parem_pool l] @ (Hashtbl.find hash vasak_pool))
;;

let loo_pooled l = List.iter (fun x -> loo_vasak_pool (tukelda x)) l

let test_string = "T → R
T → aTc
R →
R → bR";;

let rec concatParemList lis =
  match lis with
  | [] -> ""
  | h :: t -> "" ^ h.tervik ^ "|" ^ concatParemList t



let asi () = let list = gram_to_list test_string in loo_pooled list; Hashtbl.iter (fun x y -> print_endline (x ^ "->" ^ (concatParemList y) ^ "\n")) hash
 ;;