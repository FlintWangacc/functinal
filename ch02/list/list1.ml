let rec length = function
   [] -> 0
 | (a::l) -> 1 + length l

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | (a::l) -> a::append l l2

let rec rev = function
    [] -> []
  | (a::l) -> append (rev l) [a]

let rec sigma = function
    [] -> 0
  | (a::l) -> a + sigma l

let rec pi = function
    [] -> 1
  | (a::l) -> a * pi l

let rec map f l =
  match l with
    [] -> []
  | (a::l) -> f(a)::map f l

let rec flat = function
    [] -> []
  | (l::ll) -> append l (flat ll)