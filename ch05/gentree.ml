type 'a gentree = GenNode of 'a * 'a gentree list

let rec matchKey keyStr = function
  | h::t as lst -> (match h with
                    | 'a'..'z'| 'A'..'Z' ->
                      let keyHead = keyStr ^ (Char.escaped h) in
                      (match t with
                       (*| ','::sl -> (keyStr ^ (Char.escaped h), sl)*)
                       | '('::_ -> (keyStr, lst)
                       | ')'::_ -> (keyHead, [])
                       | ('a'..'z')::tl | ('A'..'Z')::tl -> (
                          match tl with
                          |('a'..'z')::tll | ('A'..'Z')::tll -> (keyHead, tl)
                          |'('::tll -> (keyHead, t)
                          |[] -> (keyHead, t)
                          )
                       | [] -> (keyHead, [])
                       | _ -> failwith "Not legal1"
                       )
                    | _ -> failwith "Not legal2"
                    )
  | [] -> (keyStr, [])

let rec matchBrack num keyStr = function
  | h::t -> (match h with
                    | '(' -> (
                      let num1 = num + 1 in
                      matchBrack num1 (keyStr^(Char.escaped h)) t
                      )
                    | ')' -> (
                      let num1 = num - 1 in
                      if num1 = 0 then
                        (keyStr^(Char.escaped h), t)
                      else
                        matchBrack num1 (keyStr^(Char.escaped h)) t
                      )
                    | elem -> matchBrack num (keyStr^(Char.escaped elem)) t  
                    )
  | [] -> (keyStr, [])

let rec gentree_list_aux strList =
  match strList with
  | '('::_ -> (
      let (brackStr, t) = matchBrack 0 "" strList in
      match t with
      | [] -> [brackStr]
      | _ -> [brackStr] @ gentree_list_aux t 
      )
  | ('a'..'z')::tlist | ('A'..'Z')::tlist-> (
    match tlist with
      | '('::_ -> (
        let hc = List.hd strList |> Char.escaped in
        let (brackStr, t) = matchBrack 0 hc tlist in
        Printf.printf "brackStr:%s\n" brackStr;
        List.iter (fun x -> Printf.printf "%c\t" x) t;
        match t with
        | [] -> [brackStr]
        | _ -> [brackStr] @ gentree_list_aux t
      )
      | _ -> (
        let (keyStr, t) = matchKey "" strList in
        List.iter (fun x -> Printf.printf "%c\t" x) t;
        print_newline (); 
        match t with
        | [] -> [keyStr]
        | _ -> [keyStr] @ gentree_list_aux t
      )
    )
  | [] -> []

let rec gentree_list str =
  let strList = String.to_seq str |> List.of_seq |> List.filter (fun t -> t != ',') in
  gentree_list_aux strList

let gentree_of_string str =
  let rec gentree_of_string_aux str =
    let strList = gentree_list str in
    let processElem elemStr =
      let strLen = String.length elemStr in
      Printf.printf "strLen:%d\n" strLen;
      let keyStr = Char.escaped (elemStr.[0]) in
      if strLen = 1 then 
        GenNode (keyStr, [])
      else
        let subKey = String.sub elemStr 2 (String.length elemStr - 3) in
        GenNode (keyStr, gentree_of_string_aux subKey)
    in
    List.map (fun t -> processElem t) strList in
  gentree_of_string_aux str |> List.hd

(*let rec gentree_of_string str =
  let strLen = String.length str in
  let isParenthesesPair str =
    str.[0] = '(' && str.[strLen-1] = ')'
  in
  match strLen with
  | 1 -> GenNode (str, [])
  | _ -> 
           let _ = Printf.printf "str:%s" str in
           let subStr = String.sub str 2 (strLen - 3) in
           let strList = String.split_on_char ',' subStr in
           let treeList = List.map (fun elemStr ->
            Printf.printf "elemStr:%s\n" elemStr;
            if String.length subStr == 1 then
              GenNode (str, [])
            else
              gentree_of_string elemStr
            ) strList in
           let keyStr = String.sub str 0 1 in
           Printf.printf "subStr:%s" subStr;
           GenNode (String.sub str 0 1, treeList)*)
 (* | _ -> (assert (isParenthesesPair str);
          if isParenthesesPair str then
          let subStr = String.sub str 1 (strLen - 1) in
          let strList = String.split_on_char ',' subStr in
          List.map (fun str ->
            if String.length str == 1 then
              GenNode(str, [])
            else
              gentree_of_string str
            ) strList
  )*)
  |(*_ -> (if isParenthesesPair str then
            let subStr = String.sub 1 (strLen - 1) in
            let strList = String.split_on_char ',' subStr in
            List.map (fun str -> gentree_of_string str) strList
         else
            failwith "error"
  )*)
let rec gentree_size = fun
  (GenNode(x, l)) -> (List.map gentree_size l) |> List.fold_left (fun t e -> t + e) 1

let rec gentree_height = fun
  (GenNode(x, l)) -> (1 + List.fold_left (fun t n -> max t n) 0
              (List.map gentree_height l))

let rec gentree_set = fun
  (GenNode(x, l)) -> [x] @ (List.fold_left (fun t x -> t @ x) [] 
              (List.map gentree_set l))

let rec flat_gentree = fun
  (GenNode (x, l)) -> x :: (List.fold_left (fun acc t -> acc @ (flat_gentree t)) [] l)

let rec gentree_hom f = fun
  (GenNode (x, l)) -> f x (List.map (gentree_hom f) l)

let gentree_size = gentree_hom (fun x l -> 1 + sigma l)

let gentree_height =
  gentree_hom (fun x 1 -> 1 + List.fold_left (fun acc t ->
                                                max acc t)
                                              0 l)
let gentree_set =
  gentree_hom (fun x l -> [x] @ (List.fold_left (List.append) [] l))

let flat_gentree =
  gentree_hom (fun x l -> x :: (List.fold_left (List.append) [] l))

let rec map_gentree f =
  gentree_hom (fun x l -> GenNode(f x, l))

let mirror_gentree =
  gentree_hom (fun x l -> GenNode (x, List.rev l))

let rec gentree_trav h g x (GenNode (a, l)) =
  h a (List.fold_right (fun t -> t |> (gentree_trav h g x) |> g) l x)

let gentree_size =
  gentree_trav (fun x y -> y + 1) (+) 0

let gentree_height =
  gentree_trav (fun x y -> y + 1) max 0

let gentree_set =
  gentree_trav (fun x l -> List.append [x] l) List.append []

let flat_gentree =
  gentree_trav List.cons List.append []

let map_gentree f =
  gentree_trav (fun x l -> GenNode (f x, l)) List.cons []

let rec do_gentree f (GenNode (x, l)) =
  f x; List.iter (do_gentree f) l

let rec at (GenNode (x, l) as t) = function
  | [] -> t
  | (i::occ) -> at (List.nth l i) occ

let rec replace_in_list lst idx entry =
  match lst with
  | h :: t -> (if idx = 0 then
                entry :: t
              else
                h::(replace_in_list t (idx - 1) entry))
  | [] -> []

let rec replace_occ (GenNode (x, l)) occ t2 =
  match occ with
  | [] -> t2
  | (i::occ) -> let ti = List.nth l i in
                         GenNode (x, replace_in_list l i (replace_occ ti occ t2))

type ty = A | B of ty |  C of ty * ty

let rec gentree_of_ty = function
  A -> GenNode ("A", [])
| (B(x)) -> GenNode ("B", [gentree_of_ty x])
| (C(x, y)) -> GenNode("C",[gentree_of_ty x; gentree_of_ty y])

type 'a signature = ('a * int) list

let arity sig1 x =
  try List.assoc x sig1
  with _ -> raise Sig_error

(*let ok_sig sig t =
  try gentree_trav (fun x bl -> List.for_all (fun b -> b = true) bl
                                & arity sig x = List.length bl)
      List.cons [] t
  with Sig_error -> false*)

type ty1 = A | B of ty1 * ty2
and ty2 = C | D of ty2 * ty1

type ('a,'b) signature = ('a * ('b list * 'b)) list

let get_type sig2 x =
  try List.assoc x sig2
  with _ -> raise Sig_error

let sig12 = ["A",([],"ty1"); "B",(["ty1";"ty2"],"ty1");
             "C",([],"ty2"); "D",(["ty2";"ty1"],"ty2")]

let typying sig2 t =
  gentree_trav (fun x ts -> let (ts1, t) = get_type sig2 x in
                            if ts = ts1 then t else raise Sig_error)
  List.cons [] t

