let rec lany l p =
  match l with
  | [] -> false
  | hd :: tl -> if p hd = true then true else lany tl p
