
#load "str.cma";;
let test = "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
";;

type disc = {
  key: string;
  children: string list option;
};;

let parseInput inputString =
  let delimLine = Str.regexp "[\n]+" in
  let parseDisc line =
    let delimWhitespace = Str.regexp "[ \t]+" in
    let delimChildren = Str.regexp "->" in
    let delimComma = Str.regexp "," in
    let tokens = Str.split delimChildren line in
    let key = (List.hd tokens) 
              |> Str.split delimWhitespace 
              |> List.hd
              |> String.trim in
    let children = if (List.length tokens > 1) 
      then Some (
          List.nth tokens 1
          |> Str.split delimComma
          |> List.map String.trim
        )
      else None in 
    {
      key;
      children;
    } in 
  let lines = Str.split delimLine inputString in
  List.map parseDisc lines;;

#trace parseInput;;

let x = parseInput test;;


