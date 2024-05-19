type 'a btree = Empty | Bin of 'a btree * 'a * 'a btree

exception Btree_exc of string

exception Match_fail

let root = function
  | Empty -> raise (Btree_exc "root: empty tree")
  | Bin (_, a, _) -> a

let left_son = function
  | Empty -> raise (Btree_exc "left_son: empty tree")
  | (Bin (t,_,_)) -> t

let right_son = function
  | Empty -> raise (Btree_exc "right_son: empty tree")
  | (Bin (_,_,t)) -> t

let rec findMatchStr idx = function
  | h :: t -> (
              match (snd h) with
              | '(' -> findMatchStr (idx + 1) t
              | ')' -> let newIdx = idx - 1 in
                       if idx = 0 then
                          fst h
                       else
                          findMatchStr newIdx t
              | ',' -> if idx = 0 then
                          fst h
                       else
                          findMatchStr idx t
              | _ -> findMatchStr idx t 

    )
  | [] -> raise Match_fail

let splitStr str =
  let startIdx = String.index str '(' in
  let endIdx = String.rindex str ')' in
  let nodeStr = String.sub str 0 startIdx in
  let subStr = String.sub str (startIdx + 1) (endIdx - startIdx - 1) in
  let strIdxLst = (String.to_seqi subStr) |> List.of_seq in
  let fstCommIdx = findMatchStr 0 strIdxLst in
  let leftStr = String.sub subStr 0 fstCommIdx in
  let rightStr = String.sub subStr (fstCommIdx+1) ((String.length subStr) - fstCommIdx - 1) in
  (nodeStr, leftStr, rightStr)

let rec btree_of_string str2node str =
  if String.exists (fun t -> t = ',') str then
    let nodeStr, leftStr, rightStr = splitStr str in
    Bin (btree_of_string str2node leftStr,
         str2node nodeStr,
         btree_of_string str2node rightStr)
  else
    Bin (Empty, str2node str, Empty)

let rec btree_hom f v t =
  match t with
  | (Bin (t1, a, t2)) -> f (btree_hom f v t1, a, btree_hom f v t2)
  | Empty -> v

let btree_height t = btree_hom (fun (x1,_,x2) -> 1 + max x1 x2) 0 t

let btree_size t = btree_hom (fun (x1,_,x2) -> 1 + x1 + x2) 0 t

let map_btree f t =
  btree_hom (fun (t1, a, t2) -> Bin (t1, f a, t2))
    Empty t

let mirror_btree t = btree_hom
                      (fun (t1, a, t2) -> Bin (t2, a, t1))
                      Empty t

let rec btree_it f t x =
  match t with
  | Empty -> x
  | Bin (t1, a, t2) ->
      btree_it f t1 (f a (btree_it f t2 x))

let rec it_btree f x t =
  match t with
  | Empty -> x
  | (Bin (t1, a, t2)) ->
      it_btree f (f (it_btree f x t1) a) t2

let flat_btree t = btree_it (fun x l -> x :: l) t []

let do_btree (f:'a -> unit) =
  let rec dof = function
    | Empty -> ()
    | (Bin (t1, a, t2)) ->
        dof t1; f a; dof t2
  in
  dof
