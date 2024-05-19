type ('a,'b) term = Term of 'a * ('a,'b) term list
                  | Var of 'b

exception Match_error
let rec matchParen strList level retLst =
  match strList with
  | '('::t -> matchParen t (level + 1) (retLst @ ["("])
  | ')'::t -> let newLevel = level - 1 in
              (if newLevel = 0 then
                (retLst @ [")"], t)
              else
                matchParen t (level - 1) (retLst @ [")"])
              )
  | h::t -> matchParen t level (retLst @ [h |> Char.escaped])
  | [] -> (if level != 0 then
              raise Match_error
          else
              (*Printf.printf "2";*)
              (retLst, []))

(*let rec matchSubStr retLst = function
  | '('::t -> let (subList, remain) = matchParen t 1 retLst in
              [subList] @ matchSubStr retLst t
  | h::t -> matchSubStr (retLst @ [Char.escaped h])  t
  | ','::t -> [retLst] @ matchSubStr [] t
  | [] -> [retLst]*)

let printStrLst msg strLst =
  print_endline msg; List.iter (fun t -> Printf.printf "%s\t" t) strLst; Printf.printf "\n"

let printCharLst msg strLst =
  print_endline msg; List.iter (fun t -> Printf.printf "%c\t" t) strLst; Printf.printf "\n"

let rec matchSubStr subChildLst subCharLst =
  printCharLst "subCharLst:" subCharLst;
  match subCharLst with
  | ','::t -> matchSubStr subChildLst t
  | h::t -> (
    match t with
    | '('::t1 ->
      (*Printf.printf "Here\n";*)
      let (par, remain) = matchParen t 0 [] in
      let ssl = (Char.escaped h)::par in
      let newsubChildLst = ssl::subChildLst in
      (*printStrLst ssl;*)
      matchSubStr newsubChildLst remain
    | ','::tl -> 
      let newsubChildLst = [Char.escaped h]::subChildLst in
      (*Printf.printf "3\n";*)
      matchSubStr newsubChildLst tl
    | [] ->
      let newsubChildLst = [Char.escaped h]::subChildLst in
      newsubChildLst
    | _ -> Printf.printf "Other:%c" h; subChildLst
    )
  | [] -> subChildLst

let rec term_of_string_aux strLst =
  let subList lst = List.filteri (fun idx _ -> idx >=2 && idx <= (List.length lst) - 2) lst in
  let toChar lst = List.map (fun t -> String.get t 0) lst in
  List.iter (printStrLst "strLst\n") strLst;
  match strLst with
  | [] -> []
  | h1::t1 -> (
    match h1 with
    | h2::t2 -> (
      match t2 with
      | [] -> (*Var (String.get h2 0)::term_of_string_aux t1*)
              Var (h2)::term_of_string_aux t1
      | _ -> let h2childLst = h1 |> subList |> toChar |> matchSubStr [] |> List.rev in
             let h2nodeLst = term_of_string_aux h2childLst in
             printStrLst "h1:" h1;
             List.iter (printStrLst "h2child\n") h2childLst;
             (*Term (String.get h2 0, h2nodeLst)::term_of_string_aux t1*)
             Term (h2, h2nodeLst)::term_of_string_aux t1
      )
    | [] -> (
      match h1 with
    | h2::t2 -> (
      match t2 with
      | [] -> (*[Var (String.get h2 0)]*)
              [Var (h2)]
      | _ -> let h2childLst = h1 |> subList |> toChar |> matchSubStr [] |> List.rev in
             let h2nodeLst = term_of_string_aux h2childLst in
             printStrLst "h1:" h1;
             List.iter (printStrLst "h2childLst:") h2childLst;
             (*Term (String.get h2 0, h2nodeLst)::term_of_string_aux t1*)
             Term (h2, h2nodeLst)::term_of_string_aux t1
      )
    )
  )
    (*match t1 with
    | [] -> [Var (String.get h1 0)]
    | h2::t2 -> (if (String.get h2 0) = '(' then
                  let childNodeLst = List.map term_of_string_aux strLst in
                  [Term (h2, childNodeLst)]
                else
                  raise Match_error
                )
      )*)

let term_of_string str =
  let strList = String.to_seq str |> List.of_seq |> List.map (Char.escaped) 
                |> (fun t -> [t]) in
  term_of_string_aux strList |> List.hd

(*let term_of_string2 str =
  let strList = String.to_seq str |> List.of_seq in
  let subList lst = List.filteri (fun idx _ -> idx >=2 && idx <= (List.length lst) - 2) lst in
  let subChildLst = List.rev (subList strList |> matchSubStr [])

let rec term_of_string str =
  let strList = String.to_seq str |> List.of_seq (*|> List.filter (fun t -> t != ',')*) in
  match strList with
  | '('::t -> matchParen strList 0 []
  | h::t -> let strLen = String.length str in
            let subStr = String.sub str 2 (strLen-2) in
            let termLst = term_of_string subStr in
            [Term (h, termLst)]*)

let rec term_trav f g start v = function
  | (Term(oper, sons)) -> f (oper, List.fold_left (fun acc t -> g (term_trav f g acc v t) start) start sons)
  | (Var n) -> v n

let vars t = term_trav snd List.append [] (fun x -> [x]) t

let occurs v t = List.mem v (vars t)

let rec print_term_str = function
  | Term (oper, sons) -> let ls = String.cat oper "(" in
                   let ssl = List.map (fun e -> print_term_str e) sons in
                   let ss = String.concat "," ssl in
                   String.cat ss ")" |> String.cat ls
  | Var n -> n

let print_term t =
  print_string  (print_term_str t)

let print_subst subst =
  List.iter (fun (x, t) -> print_string x;
                           print_string " --> ";
                           print_term t;
                           print_newline ())
  subst

let rec apply_subst subst = function
  | (Term (f, tl)) -> Term (f, (List.map (apply_subst subst) tl))
  | (Var x as v) -> try List.assoc x subst
                    with _ -> v

(*let subst = ["x", term_of_string "g(z)"]
in t11 = apply_subst subst (term_of_string "f(x,y,x)")*)

let compsubst subst1 subst2 =
  (List.map (fun (v, t) -> (v, apply_subst subst1 t)) subst2)
  @ (let vs = List.map fst subst2 in
  List.filter (fun (x, t) -> not (List.mem x vs)) subst1)

(*let subst1 = ["x",term_of_string "g(x,y)"]
and subst2 = ["y",term_of_string "h(x,z)";"x",term_of_string "k(x)"] in
print_subst (compsubst subst1 subst2)*)

exception Match_exc

let som_subst s1 s2 =
  List.fold_left (fun subst (x, t) -> try let u = List.assoc x subst in
                                     if t = u then subst
                                     else raise Match_exc
                                with Not_found -> (x,t)::subst)
  s1 s2

let matching (t1, t2) =
  let rec matchrec subst = function
  | (Var v, t) -> som_subst [v,t] subst
  | (t, Var v) -> raise Match_exc
  | (Term(op1,sons1), Term(op2,sons2)) ->
    if op1 = op2 then List.fold_left matchrec subst (List.combine sons1 sons2)
    else raise Match_exc in
    matchrec [] (t1,t2)

exception Unify_exc

let rec unify = function
  | (Var v, t2) -> if Var v = t2 then [] else
                   if occurs v t2 then raise Unify_exc
                   else [v, t2]
  | (t1, Var v) -> if occurs v t1 then raise Unify_exc
                   else [v, t1]
  | (Term (op1, sons1), Term (op2, sons2)) ->
    let subst_unif s (t1, t2) =
      compsubst (unify (apply_subst s t1, apply_subst s t2)) s in
    if op1 = op2 then
      (List.fold_left subst_unif [] (List.combine sons1 sons2))
    else raise Unify_exc

type ml_unop = Ml_fst | Ml_snd

type ml_binop = Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less 
type ml_exp =
    Ml_int_const of int
  | Ml_bool_const of bool
  | Ml_pair of ml_exp * ml_exp
  | Ml_unop of ml_unop * ml_exp
  | Ml_binop of ml_binop * ml_exp * ml_exp
  | Ml_var of string
  | Ml_if of ml_exp * ml_exp * ml_exp
  | Ml_fun of string * ml_exp
  | Ml_app of ml_exp * ml_exp
  | Ml_let of string * ml_exp * ml_exp
  | Ml_letre of string * ml_exp * ml_exp

type ml_type =
    Int_type | Bool_type
  | Pair_type of ml_type * ml_type
  | Arrow_type of ml_type * ml_type
  | Var_type of string

let var n = Var ("v"^(string_of_int n))

let const c = Term(c,[])

let pair(t1,t2) = Term("pair",[t1;t2])

let arrow(t1,t2) = Term("arrow",[t1;t2])

let (new_int,reset_new_int) =
  let c = ref (-1) in
  (fun () -> c:=!c+1; !c),
  (fun () -> c:=-1)

let unop_type = function
    Ml_fst -> let a = var(new_int()) and b = var(new_int())
              in (pair(a, b), a)
  | Ml_snd -> let a = var(new_int()) and b = var(new_int())
              in (pair(a, b), b)

let binop_type = function
  | Ml_add -> (const "int", const "int", const "int")
  | Ml_sub -> (const "int", const "int", const "int")
  | Ml_mult -> (const "int", const "int", const "int")
  | Ml_eq -> (const "int", const "int", const "bool")
  | Ml_less -> (const "int", const "int", const "bool")

let generate_type_constraints e =
  let rec gen n tenv = function
    (Ml_int_const _) -> [var n, const "int"]
  | (Ml_bool_const _) -> [var n, const "bool"]
  | (Ml_unop (op, e)) ->
      let (t1, t2) = unop_type op
      and ne = new_int () in
      (var n, t2)::(var ne, t1)::(gen ne tenv e)
  | (Ml_binop (op,e1,e2)) ->
      let (t1,t2,t3) = binop_type op
      and n1 = new_int () and n2 = new_int () in
      (var n, t3) :: (var n1, t1) :: (var n2, t2)
      :: (gen n1 tenv e1 @ gen n2 tenv e2)
  | (Ml_pair (e1, e2)) ->
      let n1 = new_int () and n2 = new_int () in
      (var n,(pair (var n1, var n2))) :: (gen n1 tenv e1 @ gen n2 tenv e2)
  | (Ml_var x) -> [ var n, List.assoc x tenv ]
  | (Ml_if (e1,e2,e3)) ->
      let n1 = new_int () and n2 = new_int ()
      and n3 = new_int () in
      (var n1, const "bool")::(var n, var n2)::(var n, var n3)
      ::((gen n1 tenv e1) @ (gen n2 tenv e2) @ (gen n3 tenv e3))
  | (Ml_fun(x,e)) ->
      let nx = new_int () and ne = new_int () in
      (var n, arrow(var nx, var ne))::
      (gen ne ((x, var n)::tenv) e)
  | (Ml_app (Ml_var "Rec", Ml_fun(f,e))) ->
    let nf = new_int () and ne = new_int () in
    (var n, var ne)::
    (gen ne ((f, var n)::tenv) e)
  | (Ml_app (e1, e2)) ->
    let n1 = new_int () and n2 = new_int () in
    (var n1, arrow(var n2, var n))::
    (gen n1 tenv e1 @ gen n2 tenv e2)
  | _ -> failwith "Not implemented" in
  reset_new_int ();gen (new_int ()) [] e

let rec ml_type_of_term = function
  | Var s -> Var_type s
  | Term ("int", []) -> Int_type
  | Term ("bool", []) -> Bool_type
  | Term ("pair", [t1;t2]) ->
      Pair_type(ml_type_of_term t1, ml_type_of_term t2)
  | Term ("arrow", [t1;t2]) ->
      Arrow_type(ml_type_of_term t1, ml_type_of_term t2)

let unify_list tl =
  let subst_unif s (t1,t2) =
    compsubst (unify (apply_subst s t1, apply_subst s t2)) s in
  List.fold_left subst_unif [] tl

let synthesize_type e =
  ml_type_of_term
   (apply_subst (unify_list (generate_type_constraints e))
    (var 0))