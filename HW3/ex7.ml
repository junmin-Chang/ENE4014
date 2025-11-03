let rec mapn f n l =
  let rec iter n f =
    match n with
    | 0 -> fun x -> x
    | _ -> fun x -> f ((iter (n - 1) f) x)
  in
  match l with
  | [] -> []
  | hd :: tl -> (iter n f) hd :: mapn f n tl
