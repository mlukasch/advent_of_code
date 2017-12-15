(* group node: {, , , , , ,} *) 
(* garbage : <....> *) 
(* garbage character cancel: !x *) 

(* Task: Transform Expression -> Tree structure *)

type tree =
  | Group of tree list
  | Garbage of string;;

exception ParseError of string;;

(* Instructions *)
(* "<>", empty garbage. *)
(* "<random characters>", garbage containing random characters. *)
(* "<<<<>", because the extra < are ignored. *)
(* "<{!>}>", because the first > is canceled. *)
(* "<!!>", because the second ! is canceled *)
(* "<!!!>>", because the second ! and the first > are canceled. *)


(* Pseudo-Code *)
(* rec parseTree seq parent *)
(* match head seq with *)
(* } -> return parent *)
(* { -> return TreeNode(parseTree(tail seq, this)) *)
(* , -> return TreeNode(this.children :: parseTree(tail seq, this)) *)
(* < -> return parseGarbage(tail seq, Garbage "") *)

(* rec parseJunk seq garbage *)
(* match head seq with *)
(* ! -> parseJunk(tail tail seq, garbage) *)
(* > -> garbage *)
(* x -> parseJunk(tail seq, garbage::x) *)
(*let explode s = String.to_list s |> List.map ~f:Char.to_string;;*)
let cl2s cl = String.concat "" (List.map (Printf.sprintf "%c") cl);;
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let rec parseGarbage seq garbage = match seq with
  | '!' :: _ :: t -> parseGarbage t garbage
  | '>' :: _ -> garbage
  | x :: t -> parseGarbage t (garbage @ [x])
  | [] -> raise (ParseError "garbage not terminating");;

let parseTree seq =
  let rec aux seq parent = match (seq, parent) with
    | ( [] , p ) -> p
    | ( '}'::t , Group p ) -> print_endline "}";Group (p @ [aux t parent])
    | ( '{'::t , _ ) -> let this = Group [] in Group [aux t this]
    | ( ','::t , Group p ) -> print_endline "komma";parent
    | ( '<'::t, _ ) -> Garbage (cl2s (parseGarbage t []))
    | ( _ :: _, _ ) -> raise (ParseError "tree parse error: invalid token") in
  let Group result = aux seq (Group []) in 
  List.hd result;;

#trace parseTree

let rec traverseTree tree leafHandler nodeHandler = match tree with
  | Group children -> 
    List.iter 
      (fun subTree -> traverseTree subTree leafHandler nodeHandler) 
      children
  | Garbage leaf -> leafHandler leaf;;

let tests1 = [
  "<>";
  "<random characters>";
  "<<<<>";
  "<{!>}>";
  "<!!>";
  "<!!!>>";
];;

let tests2 = [
  "{}";
  "{{{}}}";
  "{{},{}}";
  "{{{},{},{{}}}}";
  "{<a>,<a>,<a>,<a>}";
  "{{<ab>},{<ab>},{<ab>},{<ab>}}";
  "{{<!!>},{<!!>},{<!!>},{<!!>}}";
  "{{<a!>},{<a!>},{<a!>},{<ab>}}";
];;

let test3 = [
  "{{},{}}";
];;

let countGroups tree =
  let counter = ref 0 in
  traverseTree 
    tree 
    (fun garbage -> print_endline garbage)
    (fun node -> counter := !counter + 1);
  !counter;;

test3 |> List.iter (
  fun test -> 
    test |> explode |> parseTree |> countGroups |> 
    Printf.printf "count : %d\n"
);;

