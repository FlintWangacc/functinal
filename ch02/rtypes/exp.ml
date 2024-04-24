type exp = Constant of int
         | Variable of string
         | Addition of exp * exp
         | Multiplication of exp * exp

let rec eval env expression =
  match expression with
    (Constant n) -> n
  | (Variable x) -> env x
  | (Addition (e1, e2)) -> eval env e1 + eval env e2
  | (Multiplication (e1, e2)) -> eval env e1 * eval env e2

let rec deriv var expression =
  match expression with
    (Constant n) -> Constant 0
  | (Variable x) -> if x = var then Constant 1
                    else Constant 0
  | (Addition (e1, e2)) -> Addition(deriv var e1, deriv var e2)
  | (Multiplication (e1, e2)) -> Addition(Multiplication(e1, deriv var e2),
                                          Multiplication(deriv var e1, e2)) 