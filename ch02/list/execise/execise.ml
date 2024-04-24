(* Exercises *)

(*2.7*)

let rec polyAdd l1 l2 = match (l1, l2) with
  | (h1::t1, h2::t2) -> (h1 +. h2) :: polyAdd t1 t2
  | (h1::t1, []) -> h1::t1
  | ([], h2::t2) -> h2::t2
  | ([], []) -> []

let polyMul l1 l2 =
let rec polyMulAux l1 l2 =
  match l1 with
  | h::t -> (List.map (fun x -> x *. h) l2) :: polyMulAux t l2
  | [] -> [] in
(List.fold_left (fun t a -> polyAdd t a) []
(List.mapi (fun idx la -> (List.init idx (fun _ -> 0.) @ la)) (polyMulAux l1 l2)))

let polyEval l v =
  let rec polyEvalAux v n = function
    | h::t -> h *. (v ** n) +. polyEvalAux v (n +. 1) t
    | [] -> 0. in
  polyEvalAux v 0. l

let polydeivative l =
  List.tl (List.mapi (fun idx x -> x *. (float_of_int idx)) l)

let polyIntergel l =
  [0.] @ (List.mapi (fun idx x -> x /. ((float_of_int idx) +. 1.)) l)

(*2.8*)
type poly = {polynomial:float * float list}

let rec polyAdd2 l1 l2 =
  match (l1, l2) with
  | (h1::t1, h2::t2) -> if snd h1 = snd h2 then (fst h1 +. fst h2, snd h1) :: polyAdd2 t1 t2 else h1::polyAdd2 t1 (h2::t2)
  | ([], l2t) -> l2t
  | (l1t, []) -> l1t
  | ([], []) -> []
  (*let addPolyElem t1 t2 =
    assert (fst t1 = fst t2); (fst t1 +. fst t2, snd t1 +. snd t2) in
  List.map (fun x -> List.fold_left
                          (fun a t -> addPolyElem a t)
                          x
                          (List.find_all
                          (fun t -> fst x = fst t) l2)) 
  l1*)

let polyMul2 l1 l2 =
  let rec polyMulAux l1 l2 = match l1 with
    | h :: t -> (List.map (fun x -> (fst x *. fst h, snd x +. snd h)) l2) :: polyMulAux t l2
    | [] -> [] in
  let mulret = polyMulAux l1 l2 in
  List.filter (fun t -> not (Float.equal (fst t) 0.))
  (List.fold_left (fun a t -> polyAdd2 a t) (List.hd mulret) (List.tl mulret))

let polyEval2 l v =
  List.fold_left (fun t -> (fst t) * (Float.pow v (snd t))) 0. l

let polydeivative2 l =
  List.filter (fun t -> not (Float.equal (fst t) 0.))
  (List.map (fun t -> (fst t *. snd t, snd t -. 1.)) l)

let polyIntergel2 l =
  List.filter (fun t -> not (Float.equal (fst t) 0.))
  (List.map (fun t -> (fst t /. (snd t +. 1.), snd t +. 1.)) l)

(*2.9*)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
let rec insert e = function
  | Leaf l -> if e > l then Node (Leaf l, Leaf e)  else Node (Leaf e, Leaf l)
  | Node (l, r) -> let rec min t = match t with
                    | Leaf l -> l
                    | Node (l, r) -> min l
                    in if e < min r then Node (insert e l, r) else Node (l, insert e r)

(*2.10*)
type direction = L | R
let rec subtree d r = match (d, r) with
   | (h::t, Leaf _) -> Some r
   | (h::t, Node (left, right)) -> 
      if h = L then subtree t left
      else subtree t right
   | ([], Node (left, right)) -> Some (Node (left, right))
   | ([], Leaf r) -> Some (Leaf r)