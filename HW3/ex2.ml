let rec remove x l =
  match l with
  | [] -> []
  | hd :: tl -> if hd = x then tl else hd :: remove x tl

let rec union l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 -> h1 :: union (remove h1 t1) (remove h1 l2)
