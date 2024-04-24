type inttree = Leaf of int | Node of inttree * inttree

let rec total = function
                 | (Node (t1, t2)) -> total(t1) + total(t2)
                 | Leaf l -> l

let rec print_inttree = function
  | (Leaf n) -> print_int n
  | (Node (t1, t2))
    -> print_string "("; print_inttree t1; print_string ",";
       print_inttree t2; print_string ")"