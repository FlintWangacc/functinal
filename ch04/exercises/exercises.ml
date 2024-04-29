(* Exercise 4.7 *)

type graph = int array array

let initGraph n (*: int -> graph*) =
  Array.init n (fun idx -> Array.make 1 idx)

let addEdge g a b =
  if not (Array.memq b g.(a)) then
    g.(a) <- (Array.append g.(a) [|b|])
  else
    ()

let removeEdge g a b =
  g.(a) <- Array.of_list (List.filter (fun t -> t != b) (Array.to_list g.(a)))

