let h = fun n -> [2*n; 3*n; 5*n]

let rec flat_map f = function
    [] -> []
  | (a::l) -> f a @ flat_map f l

let rec union_map f = function
    [] -> []
  | (a::l) -> List.fold_left (fun al t -> if List.exists (fun alt -> t=alt) al then
                                            al
                                          else
                                            t::al) (f a) (union_map f l)

let rec loop p f x = if (p x) then x else loop p f (f x)

let naive_solve_breadth_first (ok,moves) start =
  List.find ok (loop (List.exists ok) (union_map moves) [start])

exception No_solution

let naive_solve_depth_first(ok,pos_moves) c =
  let rec solve_rec = function
    [] -> raise No_solution
  | (c::cl as cl') ->
      if ok c then c
      else solve_rec (pos_moves c @ cl)
  in solve_rec [c] 