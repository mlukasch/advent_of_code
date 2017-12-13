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
(* , -> return this.children :: parseTree(tail seq, this) *)
(* < -> return this.children :: parseGarbage(tail seq, Garbage "") *)

(* rec parseJunk seq garbage *)
(* match head seq with *)
(* ! -> parseJunk(tail tail seq, garbage) *)
(* > -> garbage *)
(* x -> parseJunk(tail seq, garbage::x) *)


let rec parseTree (expression:char list) (parent:tree) : tree = match (expression, parent) with
  | ([],_) -> parent
  | ('{'::t, Group group) -> Group (group @ [parseTree t (Group group)])
  | ('}'::t, Group group) -> parent
  | ('<'::t, Group group) -> ParseError

