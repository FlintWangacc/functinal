let rec list_hom e f l =
  match l with
    [] -> e
  | (a::l) -> f a (list_hom e f l)

let length = list_hom 0 (fun _ n -> n + 1)

let append l1 l2 = list_hom l2 List.cons l1

let rev = list_hom [] (fun a l -> append l [a])

let sigma = list_hom 0 (+)

let pi = list_hom 1 ( * )

let map f l = list_hom [] (fun x l -> f(x)::l) l

let flat = list_hom [] append

let rec list_it f l e =
  match l with [] -> e
            |  (a::l) -> f a (list_it f l e)

let rec it_list f e l =
  match l with [] -> e
            |  (a::l) -> it_list f (f e a) l