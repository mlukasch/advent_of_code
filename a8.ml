#load "str.cma";;
let test = "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10";;

type register = int;;
type number = int;;
type command = register * number -> int;;
type condition = (register, int) Hashtbl.t -> bool;;
type instruction = {
  condition: condition;
  command: command;
  register: register;
  number: number;
}

let parseRegister codeString : register =
  (Char.code codeString.[0]) - (Char.code 'a');;

let parseCommand cmdString : command = match cmdString with 
  | "inc" -> fun (x, y) -> x + y
  | "dec" -> fun (x, y) -> x - y
  | _ -> fun (x, y) -> x;;

let parseCondition condString: condition =
  let tokens = condString
               |> Str.split (Str.regexp "[ \t]")
               |> List.map String.trim in
  let register = parseRegister (List.hd tokens) in
  let value = int_of_string (List.nth tokens 2) in
  fun (hashtbl) ->
    let registerValue = 
      if (Hashtbl.mem hashtbl register) 
      then (Hashtbl.find hashtbl register)
      else 0 in
    match (List.nth tokens 1) with
    | ">" -> registerValue > value
    | "<" -> registerValue < value
    | ">=" -> registerValue >= value
    | "<=" -> registerValue <= value
    | "=" -> registerValue = value
    | _ -> true;;

let parseInstruction instructionString: instruction =
  let delimCond = Str.regexp "if" in
  let delimToken = Str.regexp "[ \t]" in
  let tokens = instructionString |> Str.split delimCond |> List.map String.trim in
  let condition = parseCondition (List.nth tokens 1) in
  let subTokens = Str.split delimToken (List.hd tokens) in
  let register = parseRegister (List.hd subTokens) in
  let command = parseCommand (List.nth subTokens 1) in
  let number = int_of_string (List.nth subTokens 2) in
  {
    condition;
    register;
    number;
    command;
  };;

let executeInstruction hashtbl instruction : unit =
  let { condition; register; number; command; } = instruction in
  if (condition hashtbl) then (
    let result = command (register,number) in
    Hashtbl.add hashtbl register result
  );;

let executeInstructionList instructionList : (register, int) Hashtbl.t =
  let hashtbl = Hashtbl.create 1 in
  (
    List.iter (executeInstruction hashtbl) instructionList;
    hashtbl
  );;

let findLargestNumber  (hashtbl : (register, int) Hashtbl.t) : int =
  let result = ref min_int in
  (
    Hashtbl.iter 
      (fun k v -> if v > !result then result := v else ()) 
      hashtbl;
    !result
  );;

#trace executeInstructionList  

let parseInput inputString : instruction list =
  let delimLine = Str.regexp "[\n]" in
  inputString
  |> Str.split delimLine
  |> List.map parseInstruction;;

let maxVal = parseInput test |> executeInstructionList |> findLargestNumber;;
Printf.printf "result : %d\n" maxVal;;




