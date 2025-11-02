let alterSum lst =
  let rec acc p c l =
    match l with
    | [] -> p
    | hd :: tl ->
      if c = 0 || c mod 2 = 1 then acc (p + hd) (c + 1) tl else acc (p - hd) (c + 1) tl
  in
  match lst with
  | [] -> 0
  | hd :: tl -> acc 0 0 lst
;;
