open Printf
type complex = {re_part:float; im_part:float}

let cx1 = {re_part = 1.; im_part = 0.}

let cxi = {re_part = 0.; im_part = 1.}

let complexAdd = fun {re_part = r1; im_part = i1} {re_part = r2; im_part = i2}
   -> {re_part = r1 +. r2; im_part = i1 +. i2}

let printComplex c = printf "%f + %fi\n" c.re_part c.im_part

let () = printComplex (complexAdd cx1 cxi);