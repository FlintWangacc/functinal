type 'a lnode = {info: 'a; mutable next: 'a lnode}

let mk_circular_list e =
  let rec x = {info=e; next=x} in x

let insert_head e l =
  let x = {info=e; next=l.next}
  in l.next <- x;l

let insert_tail e l =
  let x = {info=e; next=l.next}
  in l.next <-x;x


let insert_before e l =
  let lprev = l.prev in
  let x = {info=e; prev=lprev; next=l} in
  lprev.next <- x;
  l.prev <- x
  
let insert_after e l =
  let lnext = l.next in
  let x = {info=e; prev=l; next=lnext} in
  lnext.prev <- x;
  l.next <- x

let elim_head l =
  l.next <- (l.next).next;l

let last ln = ln.info

let first ln = (ln.next).info

(*let l = 
  let t1 = mk_circular_list 1 in
  List.iteri (fun t -> insert_tail t1 t) [5; 4; 3; 2]*)




type 'a queue = Emptyqueue | Queue of 'a lnode

let enqueue x = function
 | Emptyqueue -> Queue (mk_circular_list x)
 |(Queue ln) -> Queue (insert_tail x ln)

let dequeue = function
 | Emptyqueue -> failwith "dequeue: queue is empty"
 | (Queue ln) -> if ln.next == ln then (Emptyqueue,ln.info)
                 else let x = first ln in
                      (Queue (elim_head ln), x)

let list_of_queue = function
 |  Emptyqueue -> []
 | (Queue ln)
   -> let ln1 = ln.next in
      let rec loq ln =
      if ln == ln1 then []
      else ln.info :: loq ln.next in
      ln1.info :: loq ln1.next

type 'a dblnode = {info:'a; mutable prev:'a dblnode;
                   mutable next: 'a dblnode}

let mk_dbl_circular_list e =
  let rec x = {info=e; prev=x; next=x} in x

let insert_before e l =
  let lprev = l.prev in
  let x = {info=e; prev=lprev; next=l} in
  lprev.next <- x;
  l.prev <- x

let insert_after e l =
  let lnext = l.next in
  let x = {info=e; prev=l; next=lnext} in
  lnext.prev <- x;
  l.next <- x

let elim l =
  let lprev = l.prev
  and lnext = l.next
in lprev.next <- lnext;
   lnext.prev <- lprev; lprev

let elimn l =
    let lprev = l.prev
    and lnext = l.next
  in lprev.next <- lnext;
     lnext.prev <- lprev; lnext

(* Exercise 4.6 *)
let head = mk_dbl_circular_list (-1)

let init_circular lst =
  List.iter (fun t -> insert_before t head) lst

let debugList fstEntry lastEntry =
  let t = ref fstEntry in
  while !t != lastEntry.next do
    Printf.printf "%d " (!t).info;
    t := (!t).next
  done;
  (*Printf.printf "end\n";*)
  print_newline ()

let rec quicksort_dl fstEntry lastEntry =
  (*Printf.printf "quicksort_dl:%d, %d\n" fstEntry.info lastEntry.info;
  debugList fstEntry lastEntry;*)
  if fstEntry == lastEntry then
    fstEntry
  else
    let partition f l =
      let origPrev = f.prev in
      let origNext = l.next in
      let partEntry = f in
      let pe = ref partEntry in
      let t = ref f.next in
      (*let _ = Printf.printf "Partition list:";debugList f l in
      print_endline "partition:";*)
      debugList f l;
      if f == l then
        ((f, l), (f, l))
      else
        (while !t != origNext do
          if (!t).info < partEntry.info then
            insert_before (!t).info partEntry
          else
            insert_after (!t).info partEntry;
          t := elimn !t
        done;
       
        (*Printf.printf "partEntry:%d\n" partEntry.info;
        Printf.printf "uh:(%d, %d)\n" origPrev.next.info (!pe).info;
        debugList origPrev.next !pe;
        Printf.printf "bh:(%d, %d)\n" partEntry.next.info origNext.prev.info;
        debugList partEntry.next origNext.prev;*)
      (*Unix.sleep 5;*)
        (*print_endline "Partition list:";*)
        (*debugList f l;*)
      let _ = match partEntry.prev == origPrev with
        | true -> pe := partEntry
        | false -> pe := partEntry.prev in
        ((origPrev.next, !pe), (partEntry.next, origNext.prev)))
    in
    let ((upperStart, upperEnd), (bottomStart, bottomEnd)) = partition fstEntry lastEntry in
    (*Printf.printf "upperStart:%d, upperEnd:%d, bottomStart:%d, bottonEnd:%d\n" upperStart.info upperEnd.info bottomStart.info bottomEnd.info;*)
    quicksort_dl upperStart upperEnd; quicksort_dl bottomStart bottomEnd 
