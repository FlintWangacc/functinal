type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let rec search item = function
  | Leaf t -> (
              if t = item then Some t
              else None
  )
  | Node (t1, t2) -> (
    let r = search item t1 in
    match r with
    | None -> search item t2
    | Some t -> Some t
  )

let rec searchExp item = function
  | Leaf t -> (
              if t = item then Some t
              else None
  )
  | Node (t1, t2) -> (
    let r = searchExp item t1 in
    match r with
    | None -> (match searchExp item t2 with
               | Some t -> Some t
               | None -> raise (Failure "Not found"))
    | Some t -> Some t
  )