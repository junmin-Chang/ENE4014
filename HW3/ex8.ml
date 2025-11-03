type 'a ntree =
  | Leaf of 'a
  | Node of 'a ntree list

let rec findn t =
  let max x y = if x > y then x else y in
  let rec length l =
    match l with
    | [] -> 0
    | _ :: tl -> 1 + length tl
  in
  match t with
  | Leaf _ -> 0
  | Node tlist ->
    max (length tlist) (List.fold_left (fun acc t -> max acc (findn t)) 0 tlist)
