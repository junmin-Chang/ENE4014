let rec lall l p =
  match l with
  | [] -> true
  | hd :: tl -> if p hd <> true then false else lall tl p
