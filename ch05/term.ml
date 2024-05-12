type ('a,'b) term = Term of 'a * ('a,'b) term list
                  | Var of 'b

exception Match_error
let rec matchParen strList level retLst =
  match strList with
  | '('::t -> matchParen t (level + 1) (retLst @ ["("])
  | ')'::t -> let newLevel = level - 1 in
              (if newLevel = 0 then
                (retLst @ [")"], t)
              else
                matchParen t (level - 1) (retLst @ [")"])
              )
  | h::t -> matchParen t level (retLst @ [h |> Char.escaped])
  | [] -> (if level != 0 then
              raise Match_error
          else
              (*Printf.printf "2";*)
              (retLst, []))

(*let rec matchSubStr retLst = function
  | '('::t -> let (subList, remain) = matchParen t 1 retLst in
              [subList] @ matchSubStr retLst t
  | h::t -> matchSubStr (retLst @ [Char.escaped h])  t
  | ','::t -> [retLst] @ matchSubStr [] t
  | [] -> [retLst]*)

let printStrLst msg strLst =
  print_endline msg; List.iter (fun t -> Printf.printf "%s\t" t) strLst; Printf.printf "\n"

let printCharLst msg strLst =
  print_endline msg; List.iter (fun t -> Printf.printf "%c\t" t) strLst; Printf.printf "\n"

let rec matchSubStr subChildLst subCharLst =
  printCharLst "subCharLst:" subCharLst;
  match subCharLst with
  | ','::t -> matchSubStr subChildLst t
  | h::t -> (
    match t with
    | '('::t1 ->
      (*Printf.printf "Here\n";*)
      let (par, remain) = matchParen t 0 [] in
      let ssl = (Char.escaped h)::par in
      let newsubChildLst = ssl::subChildLst in
      (*printStrLst ssl;*)
      matchSubStr newsubChildLst remain
    | ','::tl -> 
      let newsubChildLst = [Char.escaped h]::subChildLst in
      (*Printf.printf "3\n";*)
      matchSubStr newsubChildLst tl
    | [] ->
      let newsubChildLst = [Char.escaped h]::subChildLst in
      newsubChildLst
    | _ -> Printf.printf "Other:%c" h; subChildLst
    )
  | [] -> subChildLst

let rec term_of_string_aux strLst =
  let subList lst = List.filteri (fun idx _ -> idx >=2 && idx <= (List.length lst) - 2) lst in
  let toChar lst = List.map (fun t -> String.get t 0) lst in
  List.iter (printStrLst "strLst\n") strLst;
  match strLst with
  | [] -> []
  | h1::t1 -> (
    match h1 with
    | h2::t2 -> (
      match t2 with
      | [] -> Var (String.get h2 0)::term_of_string_aux t1
      | _ -> let h2childLst = h1 |> subList |> toChar |> matchSubStr [] |> List.rev in
             let h2nodeLst = term_of_string_aux h2childLst in
             printStrLst "h1:" h1;
             List.iter (printStrLst "h2child\n") h2childLst;
             Term (String.get h2 0, h2nodeLst)::term_of_string_aux t1
      )
    | [] -> (
      match h1 with
    | h2::t2 -> (
      match t2 with
      | [] -> [Var (String.get h2 0)]
      | _ -> let h2childLst = h1 |> subList |> toChar |> matchSubStr [] |> List.rev in
             let h2nodeLst = term_of_string_aux h2childLst in
             printStrLst "h1:" h1;
             List.iter (printStrLst "h2childLst:") h2childLst;
             Term (String.get h2 0, h2nodeLst)::term_of_string_aux t1
      )
    )
  )
    (*match t1 with
    | [] -> [Var (String.get h1 0)]
    | h2::t2 -> (if (String.get h2 0) = '(' then
                  let childNodeLst = List.map term_of_string_aux strLst in
                  [Term (h2, childNodeLst)]
                else
                  raise Match_error
                )
      )*)

let term_of_string str =
  let strList = String.to_seq str |> List.of_seq |> List.map (Char.escaped) 
                |> (fun t -> [t]) in
  term_of_string_aux strList

let term_of_string2 str =
  let strList = String.to_seq str |> List.of_seq in
  let subList lst = List.filteri (fun idx _ -> idx >=2 && idx <= (List.length lst) - 2) lst in
  let subChildLst = List.rev (subList strList |> matchSubStr [])

let rec term_of_string str =
  let strList = String.to_seq str |> List.of_seq (*|> List.filter (fun t -> t != ',')*) in
  match strList with
  | '('::t -> matchParen strList 0 []
  | h::t -> let strLen = String.length str in
            let subStr = String.sub str 2 (strLen-2) in
            let termLst = term_of_string subStr in
            [Term (h, termLst)]

let rec term_trav f g start v = function
  | (Term(oper, sons)) -> f (oper, List.fold_left (fun acc t -> g (term_trav f g acc v t) start) start sons)
  | (Var n) -> v n

let occurs v t = List.mem v (vars t)