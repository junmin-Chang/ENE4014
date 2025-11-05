type 'a ntree =
  | Leaf of 'a
  | Node of 'a ntree list

let rec flatten nt =
  match nt with
  | Leaf n -> [ n ]
  | Node tlist ->
    let rec aux acc tlist =
      match tlist with
      | [] -> acc
      | hd :: tl -> aux (acc @ flatten hd) tl
    in
    aux [] tlist
