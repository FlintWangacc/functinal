let rec member (order, equiv) e list =
  match list with
    [] -> false
  | (a::l) -> if order(a, e) then member (order, equiv) e l
              else equiv(a, e)

let rec add_to_set (order, equiv) elem list =
  match list with
    [] -> [elem]
  | (a::l) -> if order (elem, a) then elem::a::l else
              if equiv (elem, a) then a::l
              else a::add_to_set (order, equiv) elem l

let rec inter (order, equiv) = function
  ([],_) -> []
| (_,[]) -> []
| ((a1::l1 as ll1), (a2::l2 as ll2))
  -> if equiv(a1, a2) then a1::inter (order,equiv) (l1,l2) else
     if order(a1, a2) then inter (order,equiv) (l1, ll2)
     else inter (order,equiv) (ll1, l2)

let rec union (order, equiv) = function
     ([], l2) -> l2
  |  (1, []) -> l1
  |  ((a1::l1 as ll1), (a2::l2 as ll2))
    -> if equiv(a1, a2) then a1::union (order, equiv) (l1, l2) else
       if order(a1, a2) then a1::union (order, equiv) (l1, ll2)
       else a2::union(order, equiv) (ll1, l2)