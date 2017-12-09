let findStartIndex (state):int =
  let maxIdx = ref 0 in
  let max = ref 0 in
  (
    Array.iteri (fun idx el -> if (el > !max) then (maxIdx:=idx;max:=el)) state;
    !maxIdx
  );;

let findStartIndex2 state =
  let rec aux maxIdx maxVal currentIdx =
    if currentIdx = (Array.length state)
    then maxIdx 
    else if state.(currentIdx) > maxVal 
    then aux currentIdx state.(currentIdx) (currentIdx+1) else aux maxIdx maxVal (currentIdx + 1) in
  aux 0 state.(0) 0;;

let redistribute (state: int array) =
  let maxIdx = findStartIndex2 state in 
  (
    let stackSize = state.(maxIdx) in
    (
      state.(maxIdx) <- 0;
      let rec aux state previousIdx stackSize =
        let stateSize = Array.length state in
        let currentIdx = (previousIdx + 1) mod stateSize in
        if stackSize = 0 then state else (
          state.(currentIdx) <- (state.(currentIdx) + 1);
          aux state currentIdx (stackSize-1)
        )
      in aux state maxIdx stackSize
    )
  );;

let debug state = 
  let rec aux stateStore currentState cycleCount =
    let newState = redistribute state in
    if List.mem newState stateStore 
    then (
      cycleCount + 1
    )
    else aux ((Array.copy newState)::stateStore) newState (cycleCount  + 1) in
  aux [] state 0

let tests = 
  [
    [|0;2;7;0|] ;
    [|2;4;1;2|] ;
    [|3;1;2;3|] ;
    [|0;2;3;4|] ;
    [|1;3;4;1|] ;
  ]
;;

let test = [|0;2;7;0|] ;;

let printResult array = 
  Array.iter (Printf.printf "|%d|") array;
  print_endline "";;

let parseInput inputStr =
  let splitRegex = Str.regexp "[ \t]+" in
  inputStr |> Str.split splitRegex |> List.map int_of_string |> Array.of_list;;

(* tests |> List.iter (fun arr -> arr |> redistribute |> printResult);;*)

(* test |> debug |> Printf.printf "result : %d\n";; *)

let inputStr = "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5";;
inputStr |> parseInput |> debug |> Printf.printf "result : %d\n";;


