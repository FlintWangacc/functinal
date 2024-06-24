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

let rec loop p f x = (*let test = p x in Printf.printf "%B\n" test;*)if p x then x else loop p f (f x)

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

let rec select p = function
    [] -> []
  | (a::l) -> if p a then a::select p l
              else select p l

type comparison = Smaller | Equiv | Greater
type 'a archive = {archlist:'a list; comp:'a->'a->comparison}

let add_to_archive arch elem =
  if List.exists (fun t -> arch.comp t elem == Equiv) arch.archlist then
    arch
  else
    { archlist = elem::arch.archlist; comp = arch.comp }

let add_list_to_archive:'a archive -> 'a list -> 'a archive = fun arch elemlst ->
  List.fold_left (fun a t -> add_to_archive a t) arch elemlst

let make_archive comp lst =
  let arch = {archlist = []; comp = comp} in
  add_list_to_archive arch lst

let archive_member elem arch =
  List.exists (fun t -> arch.comp t elem = Equiv) arch.archlist

let archive_map arch_part f (arch,l) =
  let rec arch_map arch ll = function
    [] -> (arch,ll)
  | (c::cl) ->
      let ll' = select
                  (fun c -> not (archive_member (arch_part c) arch)) (f c) in
      arch_map (add_list_to_archive arch (List.map arch_part ll')) (ll'@ll) cl in
  arch_map arch [] l

(*let archive_map arch_part f (s, l) = 
  let rec arch_map s ll = function
    [] -> (s, ll)
  | (c::l) -> let ll' = select (fun c -> not (set_member c s)) (f c) in
              arch_map (add_list_to_set s ll') (ll'@ll) l in
  arch_map s [] l*)

let (>>) f g x = g(f(x))

let (<<) f g x = f(g(x))

(*let solve_breadth_first' (ok, moves, comp) start =
  (List.find ok << snd) (loop (List.exists ok << snd)
                          (archive_map moves)
                          (make_set comp [start],[start]))*)
let solve_breadth_first (ok,pos_moves,comp) arch_part start =
  (List.find ok << snd)
    (loop
      (List.exists ok << snd)
      (archive_map arch_part pos_moves)
      (make_archive comp (List.map arch_part start),start))

(*let explore_breadth_first (pos_moves,comp) arch_map start =
  fst (loop
        (fun (s,l) -> l = [])
        (archive_map arch_map pos_moves)
        (make_archive comp (map arch_map start),start))*)

let explore_breadth_first (pos_moves,comp) arch_map start =
  fst (loop
        (fun (s,l) -> l=[])
        (archive_map arch_map pos_moves)
        (make_archive comp (List.map arch_map start),start))

let explore_depth_first (pos_moves,comp) arch_part c =
  let rec solve_rec a = function
    | [] -> a
    | (c::cl) ->
        if archive_member (arch_part c) a then solve_rec a cl
        else solve_rec (add_to_archive a (arch_part c) )
                (pos_moves c @ cl) in
  solve_rec (make_archive comp []) [c]

let up n = n - 10 and left n = n - 1
and down n = n + 10 and right n = n + 1

type piece = Donkey | Square | Horiz | Vertic | Notany

type board =
     {donkey: int;
      squares: int list;
      horiz: int;
      vertics: int list}

let print_board ppf ({donkey:int; squares:int list; horiz: int; vertics: int list}) =
  let donkey_str = Printf.sprintf "donkey:%d" donkey in
  let horiz_str = Printf.sprintf "horiz:%d" horiz in
  let int_list_to_str = List.fold_left (fun acc x -> acc ^ string_of_int x ^ " ") "" in
  let squares_str = Printf.sprintf "squares:%s" (int_list_to_str squares) in
  let vertics_str = Printf.sprintf "vertics:%s" (int_list_to_str vertics) in
  let str = donkey_str ^ ";" ^ squares_str ^ ";" ^ horiz_str ^ ";" ^ vertics_str in
  Format.fprintf ppf "%s" str

(*#install_printf print_board*)

let start =
  { donkey = 12;
    squares = [11;14;21;24];
    horiz = 32;
    vertics = [41;42;43;44]}

type direction = Left | Up | Right | Down
type move = int * direction

let print_move ppf (pos, direct) =
  let directStr = match direct with
    Left -> "left"
  | Up -> "up"
  | Right -> "right"
  | Down -> "down" in
  Format.fprintf ppf "(%d, %s)" pos directStr

(*#install_printf print_move*)

let dumpMove t =
  let dir = match (snd t) with
  | Left -> "left"
  | Up -> "up"
  | Right -> "right"
  | Down -> "down" in
  Printf.printf "(%d, %s)\n" (fst t) dir

let dumpBoard board =
  assert (List.length board.squares == 4);
  Printf.printf "donkey:%d" board.donkey; print_newline ();
  Printf.printf "squares:"; print_newline ();
  List.iter (fun t -> Printf.printf "%d," t) board.squares; print_newline ();
  print_newline ();
  Printf.printf "horiz:%d" board.horiz; print_newline ();
  Printf.printf "vertics:";
  List.iter (fun t -> Printf.printf "%d," t) board.vertics; print_newline ()

let donkeylst donkey = [donkey; right donkey; down donkey; (right<< down) donkey]
let getVertic num = [num; down num]
let horizlst horiz = [horiz; right horiz]
let getVerticList vet = List.fold_left (fun acc t -> acc @ t) [] (List.map getVertic vet)
let get cell board =
  (*let _ = Printf.printf "\nget_cell:%d\n" cell in*)
  (*let _ = dumpBoard board in*)
  if List.mem cell (donkeylst board.donkey) then Donkey else
  if List.mem cell (horizlst board.horiz) then Horiz else
  if List.mem cell board.squares then Square else
  if List.mem cell (getVerticList board.vertics) then Vertic
  else Notany

(*let moves1 b board =
  (match get (right b) board with
    (Square | Horiz) -> [(b,Left)] | _ -> []) @
  (match get (down b) board with
    (Square | Vertic) -> [(b,Up)] | _ -> []) @
  (match (get (left b) board, get (left(left b)) board) with
    ((Square,_)|(_,Horiz)) -> [(b,Right)]| _ -> []) @
  (match (get (up b)board, get (up(up b)) board) with
    ((Square,_)|(_,Vertic)) -> [(b,Down)] | _ -> [])*)

let moves1 b board =
  (match get (right b) board with
    (Square | Horiz) -> [(b, Left)] | _ -> []) @
  (match get (down b) board with
    (Square | Vertic) -> [(b, Up)] | _ -> []) @
  (match get (left b) board with
    (Square | Horiz) -> [(b, Right)]| _ -> []) @
  (match get (up b) board with
    (Square | Vertic) -> [(b,Down)] | _ -> [])

let h_adjacent (n1,n2) = n2 = n1 + 1
let v_adjacent (n1,n2) = n2 = n1 + 10

(*let moves2 (b,b') board =
  if not(h_adjacent(b,b')) && not(v_adjacent(b,b')) then [] else
  if h_adjacent (b,b') then
    (match (get (up b) board, get (up (up b)) board) with
      ((_,Donkey)|(Horiz,_)) -> [(b,Down)]|_ -> []) @
    (match get (down b) board with
      (Donkey | Horiz) -> [(b,Up)] | _ -> [])
  else
    (match (get (left b) board, get (left (left b)) board) with
      ((_,Donkey)|(Vertic,_)) -> [(b,Right)] | _ -> []) @
    (match get (right b) board with
      (Donkey|Vertic) -> [b,Left]| _ -> [])*)

let moves2 (b,b') board =
  let getMainVect m = List.find (fun t -> t = m || m = down t) board.vertics in
  if not(h_adjacent(b,b')) && not(v_adjacent(b,b')) then [] else
  if h_adjacent (b, b') then
    (match (get (up b) board, get (up b') board) with
      (Donkey, Donkey | Horiz, Horiz) -> [(b, Down)]|_ -> []) @
    (match (get (down b) board, get (down b') board) with
      (Donkey, Donkey | Horiz, Horiz) -> [(b, Up)] | _ -> [])
  else
    (match (get (left b) board, get (left b') board) with
      (Donkey,Donkey) -> [(b, Right)] | (Vertic, Vertic) -> if getMainVect (left b) = getMainVect (left b') then [(b, Right)] else [] | _ -> []) @
    (match (get (right b) board, get (right b') board) with
      (Donkey,Donkey) -> [(b, Left)] | (Vertic, Vertic) -> if getMainVect (right b) = getMainVect (right b') then [(b, Left)] else [] | _ -> [])

let computeEmpty board =
  (*let _ = Printf.printf "Enter\n" in
  let _ = dumpBoard board in*)
  let numlist = [11; 12; 13; 14; 21; 22; 23; 24; 31; 32; 33; 34; 41; 42; 43; 44; 51; 52; 53; 54] in
  let getDonkey num = [num; right num; down num; (right << down) num] in
  let getHoriz num = [num; right num] in
  let getVertic num = [num; down num] in
  let getVerticList vet = List.fold_left (fun acc t -> acc @ t) [] (List.map getVertic vet) in
  let allList = (getDonkey board.donkey) @ (getHoriz board.horiz) @ (getVerticList board.vertics) @ board.squares in
  let removeSquare = List.filter (fun t -> not (List.mem t allList)) numlist in
  assert (List.length removeSquare = 2);
  (List.nth removeSquare 0, List.nth removeSquare 1)

let dumpStatus ((b1, b2), c) =
  Printf.printf "Empty:%d, %d\n" b1 b2;
  dumpBoard c

let getMoveBoard p board =
  let bvts = List.map (fun t -> [t; down t]) board.vertics in
  match (get p board) with
    Donkey -> board.donkey
  | Horiz -> board.horiz
  | Square -> p
  | Vertic -> let vt = List.find (fun vs-> List.exists (fun t -> t = p) vs) bvts in
              List.nth vt 0

let app_move b1 dir =
  let empty1,empty2 = fst b1 in
  let board = snd b1 in
  let fille,direction = dir  in
  let trans,reverse,movep = match direction with Left -> left,right,(right fille) | Up -> up,down,(down fille) | Right -> right,left,(left fille) | Down -> down,up,(up fille) in
  let movep = getMoveBoard movep board in
  (*let _ = Printf.printf "%d" movep in*)
  (*let _ = Printf.printf "\nfille:%d, movep:%d" fille movep in
  let _ = dumpMove dir in*)
  let movetype = get (reverse fille) board in
  let newboard = match movetype with
    Donkey -> {board with donkey = trans board.donkey}
  | Horiz -> {board with horiz = trans board.horiz}
  | Vertic -> let newVert = (trans movep)::(List.filter (fun t -> t!= movep) board.vertics) in {board with vertics = newVert}
  | Square -> let newSq = (trans movep)::(List.filter (fun t -> t!= movep) board.squares) in {board with squares = newSq} in
  (*let _ = Printf.printf "\n(%d, %d)\n" empty1 empty2 in
  let _ = dumpBoard board in
  let _ = dumpStatus b1;dumpMove dir in
  let _ = dumpBoard board in*)
  assert (List.length board.squares = 4 && List.length board.vertics = 4);
  computeEmpty newboard, newboard

let app_moves c = List.fold_left (fun board move -> app_move board move) c


let next_configs (ml, (((b1, b2), c) as c')) =
  (*let _ = Printf.printf "next_configs\n" in*)
  (*let _ = Printf.printf "\nb1:%d, b2:%d\n" b1 b2 in*)
  let moves = moves1 b1 c @ moves1 b2 c @ moves2 (b1,b2) c in
  (*let _ = List.iteri (fun i m -> if i < ((List.length moves) - 1) then (dumpMove m; Printf.printf ",") else (dumpMove m; Printf.printf "\n")) moves in*)
  (*let _ = dumpStatus c' in*)
  List.map (fun m -> (m::ml, app_move c' m)) moves

(*let donkey_comp b1 b2 =
  let e1 = computeEmpty b1 in
  let e2 = computeEmpty b2 in
  if fst e1 < fst e2 then Smaller else
  if (fst e1 = fst e2) && (snd e1 < snd e2) then Smaller else
  if (fst e1 = fst e2) && (snd e1 = snd e2) then Equiv else
  if (fst e1 = fst e2) && (snd e1 > snd e2) then Greater else
  Greater*)

let donkey_comp b1 b2 =
  let int_comp v1 v2 =
  match v1 < v2, v1 > v2 with
    true, _ -> Smaller
  | _, true -> Greater
  | false, false -> Equiv
  in
  match int_comp b1.donkey b2.donkey,int_comp b1.horiz b2.horiz with
    Smaller,_ -> Smaller
  | Greater,_ -> Greater
  | Equiv, Smaller -> Smaller
  | Equiv, Greater -> Greater
  | Equiv, Equiv -> (
      let rec list_comp =function
        (h1::t1),(h2::t2) -> let r = int_comp h1 h2 in
                             if r = Equiv then list_comp (t1,t2) else r
      | [],[] -> Equiv
      | _, _ -> assert false in
      match list_comp (b1.vertics,b2.vertics) with
      | Smaller -> Smaller
      | Greater -> Greater
      | Equiv -> int_comp b1.squares b2.squares   
  )

(*let donkey_comp b1 b2 = 
  let list_eqaul = List.equal (fun a b -> a = b) in
  if b1.donkey = b2.donkey && b1.horiz = b2.horiz && 
     list_eqaul b1.squares b2.squares && list_eqaul b1.vertics b2.vertics then
      Equiv
  else
    Smaller*)

let solve_red_donkey start ok =
  solve_breadth_first
    (ok, next_configs, donkey_comp)
    (snd << snd)
    [([], start)]

let ok_config c = (snd(snd c)).donkey / 10 = 4

let start = ((31,34), start)
let ms = List.rev (fst (solve_red_donkey start ok_config))
