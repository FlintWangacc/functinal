(* Exercise *)
(* 1.2 *)
let derivitive = fun f x dx -> (f(x +. dx) -. f(x)) /. dx

(* 1.3 *)
let smoothing = fun f x dx -> (f(x -. dx) +. f(x +. dx) +. f(x)) /. 3.

(* 1.4 *)
let rec power a n =
  if n = 0 then 1 else a * power a (n-1)

(* 1.4-2 *)
let rec power2 a n =
  if n = 1 then
    a
  else if n % 2 = 0 then
    let t = power2 a (n / 2)
    in t * t
  else
    a * power2 a (n-1)

(* 1.5 *)
let rec gcd a b =
  match a with
  | 0 -> b
  | _ -> (if b > a then
            gcd a (b-a)
          else
            gcd b (a-b)
          )

(* 1.6 *)
let isPrime n =
  let fact = List.init (n/2) (fun i -> i + 2) in
  not (List.exists fact (fun e -> print_int e; n % e = 0))

(* 1.7 *)
let avbvc = function
  | (false, false, false) -> false
  | _ -> true


    