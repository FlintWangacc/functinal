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

type comparison = Smaller | Equiv | Greater

let mk_order ord x y =
  if ord x y then Smaller else
  if x = y then Equiv
  else Greater
let int_comp = mk_order (<)

let mk_preorder (lt,eq) x y =
  if lt x y then Smaller else
  if eq x y then Equiv
  else Greater

type 'a minmax = Min | Plain of 'a | Max

let extend_order ord x y =
  match (x,y) with
  | (Min, Min) | (Max, Max) ->Equiv
  | (Min, _) | (_, Max) -> Smaller
  | (Max, _) | (_, Min) -> Greater
  | (Plain x, Plain y) -> ord x y

let rec is_bst order =
  let rec check_bst (a, b) t =
    let ext_order = extend_order order in
    match t with
    | Empty -> true
    | (Bin (t1, x, t2)) ->
        let x = Plain x in
        not (ext_order a x = Greater) &&
        not (ext_order x b = Greater) &&
        check_bst (a, x) t1 &&
        check_bst (x, b) t2
  in
  check_bst (Min, Max)

exception Bst_search_exc of string

let search_bst order answer e =
  let rec search = function
      Empty -> raise (Bst_search_exc "search_bst")
    | (Bin(t1,x,t2) as t) ->
        match order e x with
          Equiv -> answer t
        | Smaller -> search t1
        | Greater -> search t2
  in
  search

let find_bst order = search_bst order root

let belongs_to_bst order e t =
  try find_bst order e t; true
  with Bst_search_exc _ -> false

let change_bst order modify e =
  let rec change = function
    Empty -> raise (Bst_search_exc "change_bst")
  | (Bin (t1,x,t2) as t) ->
      (match order e x with
        Equiv -> Bin (t1, modify x, t2)
      | Smaller -> Bin (change t1, x, t2)
      | Greater -> Bin (t1, x, change t2))
  in change

let rec add_bottom_to_bst option order t e =
  let rec add = function
    | Empty -> Bin (Empty, e, Empty)
    | (Bin (t1, x, t2) as t) ->
          (match order e x with
             Equiv -> Bin(t1, option e x, t2)
           | Smaller -> Bin(add t1, x, t2)
           | Greater -> Bin(t1, x, add t2)) in
  add t

let add_list_bottom_to_bst option order =
  List.fold_left (add_bottom_to_bst option order)

let mk_bst option order = add_list_bottom_to_bst option order Empty

let t = mk_bst (fun x y -> x) int_comp [10;15;12;4;6;21;8;1;17;2]

(*let rec cut order e x = 
  match x with
  | Empty -> (Empty, e, Empty)
  | (Bin (t1,a,t2)) ->
        (print_int a; print_newline ();match order e a with
          Smaller -> let (t,e',t') = cut order e t1 in
                     (t, e', Bin(t',a, t2))
        | Equiv -> (t1, a, t2)
        | Greater -> let (t, e', t') = cut order e t2 in
                      (Bin(t1,a,t),e',t'))
let cut_bst order e =
  cut order e*)

let rec cut_bst order e =
  let rec cut = function
    Empty -> (Empty, e, Empty)
  | (Bin (t1,a,t2)) ->
      (match order e a with
        Smaller -> let (t,e',t') = cut t1 in
                   (t, e', Bin(t',a,t2))
      | Equiv -> (t1,a,t2)
      | Greater -> let (t,e',t') = cut t2 in
                    (Bin (t1,a,t),e',t'))
  in cut

let add_root_to_bst option order t e =
  let t1,e',t2 = cut_bst order e t in
  Bin(t1, option e e', t2)

let add_list_root_to_bst option order =
  List.fold_left (add_root_to_bst option order)

let mk_bst2 option order = add_list_root_to_bst option int_comp Empty

let t2 = mk_bst2 (fun x y -> x) int_comp [10;15;12;4;6;21;8;1;17;2]

exception Bst_exc of string

let rec remove_biggest = function
  | (Bin(t1,a,Empty)) -> (a,t1)
  | (Bin(t1,a,t2)) ->
      let (a',t') = remove_biggest t2 in (a', Bin(t1,a,t'))
  | Empty -> raise (Bst_exc "remove_biggest: tree is empty")

let rec rem_root_from_bst = function
    Empty -> raise (Bst_exc "rem_root_from_bst: tree is empty")
  | (Bin(Empty,a,t2)) -> t2
  | (Bin (t1,_,t2)) ->
      let (a',t') = remove_biggest t1 in
      Bin(t',a',t2)

let rec rem_from_bst order e =
  let rec rem = function
    | Empty -> raise (Bst_search_exc "rem_from_bst")
    | (Bin (t1,a,t2) as t) ->
        (match order e a with
          Equiv -> rem_root_from_bst t
        | Smaller -> Bin(rem t1, a, t2)
        | Greater -> Bin(t1,a, rem t2))
  in rem

let rem_list_from_bst order btree lst =
  List.fold_left (fun btree e -> rem_from_bst order e btree) btree lst

let rot_right = function
    Bin(Bin(u, p, v),q, w) -> Bin(u, p, Bin(v,q,w))
  | _ -> raise (Btree_exc "rot_right")

let rot_left = function
    (Bin(u, p, Bin(v, q, w))) -> Bin(Bin(u,p,v), q, w)
  | _ -> raise (Btree_exc "rot_left")

let rot_left_right = function
    (Bin(Bin(t,p,Bin(u,q,v)),r,w)) -> Bin(Bin(t,p,u),q, Bin(v,r,w))
  | _ -> raise (Btree_exc "rot_left_right")

let rot_right_left = function
    (Bin(t,p,Bin(Bin(u,q,v),r,w))) -> Bin(Bin(t,p,u),q,Bin(v,r,w))
  | _ -> raise (Btree_exc "rot_right_left")