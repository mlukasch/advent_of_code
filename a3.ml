let squareSize x = 
  let rec aux acc n = 
    if acc * acc > n then acc else aux (acc + 2) n
  in aux 1 x;;

let innerSquareSize x =
  let size = squareSize x in 
  ((size - 2 ) * (size - 2));;

let diff x = x - innerSquareSize x;;

let rest x = 
  let diff = diff x in 
  let squareSize =  squareSize x in
  Printf.printf "diff : %d\n" diff; 
  Printf.printf "squareSize : %d\n" squareSize; 
  diff mod (squareSize - 1);;

let steps x = 
  let center = ((squareSize x) + 1) / 2 in
  let rest = rest x in
  Printf.printf "rest : %d\n" rest;
  let coord1 = if rest + 1 > center then rest + 1 - center else center - rest - 1 in
  let coord2 = center -1 in
  Printf.printf "coord1 : %d\n" coord1;
  Printf.printf "coord2 : %d\n" coord2;
  coord1 + coord2;;

(*
Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.
*)

Printf.printf "steps : %d\n" (steps 368078);;