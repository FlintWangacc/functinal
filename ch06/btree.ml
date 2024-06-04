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

type balance = Left | Balanced | Right

type 'a avltree = ('a * balance) btree

exception Avl_exc of string

exception Avl_search_exc of string

let belongs_to_avl order =
  belongs_to_bst (fun x y -> order x (fst y))

let find_avl order e (t:('a * balance) btree) =
  try fst(find_bst (fun x y -> order x (fst y)) e t)
  with Bst_search_exc _ -> raise (Avl_search_exc "find_avl")

let h_balanced (t:('a*balance) btree) =
  let rec correct_balance = function
    Empty -> 0
  | (Bin(t1,(x,b),t2)) ->
    let n1 = correct_balance t1 and n2 = correct_balance t2 in
    if (b=Balanced && n1 = n2) then n1 + 1 else
    if (b=Left && n1 = n2 + 1) then n1 + 1 else
    if (b=Right && n2 = n1 + 1) then n2 + 1
    else raise (Avl_exc "not avl") in
  try correct_balance t; true
  with Avl_exc _ -> false

let is_avl order (t:('a*balance) btree) =
  h_balanced t && is_bst (fun x y -> order (fst x) (fst y)) t

let mirror_avl t =
  btree_hom
    (fun (t1,(x,b),t2)
      -> let b' = match b with
           Left -> Right
         | Balanced ->Balanced
         | Right -> Left in
         Bin(t2,(x,b'),t1))
  Empty t

exception Avl_rotation_exc of string

let rot_right = function
  (Bin(Bin(u,(p,b),v),(q,_),w)) ->
    (match b with
      Balanced -> Bin(u,(p,Right),Bin(v,(q,Left),w))
    | Left -> Bin(u,(p,Balanced),Bin(v,(q,Balanced),w))
    | Right -> raise (Avl_rotation_exc "rot_right"))
  | _ -> raise (Avl_rotation_exc "rot_right")

type avl_add_info = No_inc | Incleft | Incright

let rec add_to_avl option order t e =
  let rec add = function
    Empty -> Bin(Empty,(e,Balanced),Empty),Incleft
  | (Bin(t1,(x,b),t2) as t) ->
      (match (order e x, b) with
        (Equiv,_) -> Bin(t1, (option e x,b), t2),No_inc
      | (Smaller,Balanced) ->
          let t,m = add t1 in
          if m = No_inc then Bin(t,(x,Balanced),t2),No_inc
          else Bin(t,(x,Left),t2),Incleft
      | (Greater,Balanced) ->
          let t,m = add t2 in
          if m = No_inc then Bin(t1,(x,Balanced),t),No_inc
          else Bin(t1,(x,Right),t),Incright
      | (Greater,Left) ->
          let t,m = add t2 in
          if m = No_inc then Bin(t1,(x,Left),t),No_inc
          else Bin(t1,(x,Balanced),t),No_inc
      | (Smaller,Left) ->
          let t,m = add t1 in
          (match m with
            No_inc -> Bin(t,(x,Left),t2),No_inc
          | Incleft -> rot_right(Bin(t,(x,Balanced),t2)),No_inc
          | Incright -> rot_left_right(Bin(t,(x,Balanced),t2)),No_inc)
      | (Smaller,Right) ->
          let t,m = add t1 in
          if m = No_inc then Bin(t,(x,Right),t2),No_inc
          else Bin(t,(x,Balanced),t2),No_inc
      | (Greater,Right) ->
          let t,m = add t2 in
          (match m with
            No_inc -> Bin(t1,(x,Right),t),No_inc
          | Incleft -> rot_right_left(Bin(t1,(x,Balanced),t)),No_inc
          | Incright -> rot_left(Bin(t1,(x,Balanced),t)),No_inc))
      in fst(add t)

let add_list_to_avl option order = List.fold_left (add_to_avl option order)

let mk_avl option order = add_list_to_avl option order Empty

let do_avl f bt = do_btree (fun (t,_) -> f t) bt

let merge_avl option order =
  it_btree (fun t x -> add_to_avl option order t (fst x))

let flat_avl t = btree_it (fun x l -> fst x :: l) t []

let avl_sort order lst =
  flat_avl @@ mk_avl (fun x y -> x) order lst

type avl_rem_info = No_dec | Dec

let balance = function
    (Bin (_,(_,b),_)) -> b
  | Empty -> Balanced

let balance_right (t,x,t') =
  match balance t with
    (Left | Balanced) -> rot_right (Bin(t,(x,Balanced),t'))
  | Right -> rot_left_right (Bin(t,(x,Balanced),t'))

let balance_left (t,x,t') =
  match balance t' with
    (Right | Balanced) -> rot_left (Bin(t,(x,Balanced),t'))
  | Left -> rot_right_left (Bin(t,(x,Balanced),t'))

let rec avl_remove_biggest = function
    (Bin(t1,(a,_),Empty)) -> (a,t1,Dec)
  | (Bin(t1,(a, Balanced), t2)) ->
      let (a',t',b) = avl_remove_biggest t2 in
      (match b with
        Dec -> (a', Bin(t1,(a,Left),t'),No_dec)
      | No_dec -> (a', Bin(t1,(a, Balanced),t'),No_dec))
  | (Bin(t1,(a,Right),t2)) ->
      let (a',t',b) = avl_remove_biggest t2 in
      (match b with
        Dec -> (a', Bin(t1,(a,Balanced),t'),Dec)
      | No_dec -> (a', Bin(t1, (a,Right),t'),No_dec))
  | (Bin(t1,(a,Left),t2)) ->
      let (a',t',b) = avl_remove_biggest t2 in
      (match b with
        Dec -> (a', balance_right (t1,a,t'),
                match snd(root t1) with
                 (Left|Right) -> Dec
                | Balanced -> No_dec)
      | No_dec -> (a', Bin(t1, (a,Right),t'), No_dec))

let rec remove_from_avl order t e =
  let rec remove = function
    Empty -> raise (Avl_search_exc "remove_from_avl")
  | (Bin(t1,(a,b),t2)) ->
      match order e a with
        Equiv ->
          if t1 = Empty then t2,Dec else
          if t2 = Empty then t1,Dec else
          let (a',t',m) = avl_remove_biggest t1 in
          (match m with
            No_dec -> Bin(t',(a',b),t2),No_dec
          | Dec -> (match b with
                      Balanced -> Bin(t',(a',Right),t2),No_dec
                    | Left -> Bin(t',(a',Balanced),t2),Dec
                    | Right -> balance_left(t',a',t2),
                               if balance t2 = Balanced
                              then No_dec else Dec))
      | Smaller ->
        let t',m = remove t1 in
        (match m with
          No_dec -> Bin(t',(a,b),t2),No_dec
        | Dec -> (match b with
                    Balanced -> Bin(t',(a,Right),t2),No_dec
                  | Left -> Bin(t',(a,Balanced),t2),Dec
                  | Right -> balance_left(t',a,t2),
                             if balance t2 = Balanced
                             then No_dec else Dec))
      | Greater ->
        let t',m = remove t2 in
        (match m with
          No_dec -> Bin(t1,(a,b),t'),No_dec
        | Dec -> (match b with
                    Balanced -> Bin(t1,(a,Left),t'),No_dec
                  | Right -> Bin(t1,(a,Balanced),t'),Dec
                  | Left -> balance_right(t1,a,t'),
                            if balance t1 = Balanced
                            then No_dec else Dec))
 in
  fst (remove t)

type ('a,'b) dictionary =
    { dict_rel: 'b -> 'b -> comparison;
      dict_data: ('a * 'b) avltree}

let dict_assoc e d =
  fst(fst(find_avl (fun x y -> d.dict_rel x (snd y)) e d.dict_data))

let dict_add_or_replace {dict_rel=c;dict_data=t} e =
  {dict_rel=c;
   dict_data = add_to_avl (fun x y -> y) (fun x y -> c (snd x) (snd y)) t e}

let dict_remove {dict_rel=c;dict_data=t} key =
  {dict_rel=c;
   dict_data=remove_from_avl (fun x y -> c x (snd y)) t key}

let dict_merge opt d1 d2 =
  if not (d1.dict_rel == d2.dict_rel) then
    failwith "dict_merge:dictionaries have different orders" else
  {dict_rel = d1.dict_rel;
   dict_data = merge_avl opt (fun x y -> d1.dict_rel (snd x) (snd y))
             d1.dict_data d2.dict_data}

type 'a set =
   {set_elements: 'a avltree;
    set_order: 'a -> 'a -> comparison}

let make_set c l =
  {set_elements = mk_avl (fun x y -> x) c l;
   set_order = c}

let set_isempty s =
  (s.set_elements = Empty)

let set_member x s =
  belongs_to_avl s.set_order x s.set_elements

let set_it f s =
  btree_it (fun x y -> f (fst x) y) s.set_elements

let it_set f x s =
  it_btree (fun x y -> f x (fst y)) x s.set_elements

let do_set f s = do_avl f s.set_elements

exception Set_exc of string

let set_forall p s =
  try
    do_set (fun x -> if not (p x)
                     then raise (Set_exc "")) s;
    true
  with  Set_exc _ -> false

let set_exists p s =
  try
    do_set (fun x -> if (p x)
                     then raise (Set_exc "")) s;
    false
  with Set_exc _ -> true

let sub_set s1 s2 =
  set_forall (fun e -> set_member e s2) s1

let set_equiv s1 s2 =
  (sub_set s1 s2) && (sub_set s2 s1)

let list_of_set s =
  flat_avl s.set_elements

let set_random_element s =
  fst (root s.set_elements)

let add_to_set s x =
  {set_elements = add_to_avl (fun x y -> x) s.set_order s.set_elements x;
   set_order = s.set_order}

let add_list_to_set s l =
  List.fold_left add_to_set s l

let remove_from_set s x =
  try {set_elements = remove_from_avl s.set_order s.set_elements x;
       set_order = s.set_order}
  with _ -> raise (Set_exc "remove_from_set")

let remove_list_from_set s = List.fold_left remove_from_set s

let substract_from_set s x =
  try remove_from_set s x
  with _ -> s

let set_union s1 s2 =
  if not(s1.set_order = s2.set_order)
  then raise (Set_exc "set_union: different set orders")
  else it_set add_to_set s1 s2

let set_diff s1 s2 =
  if not(s1.set_order = s2.set_order)
  then raise (Set_exc "set_diff: different set orders")
  else it_set substract_from_set s1 s2

let set_intersection s1 s2 = set_diff s1 (set_diff s1 s2)