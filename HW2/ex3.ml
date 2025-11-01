let rec min lst =
  match lst with
  | [] -> 0
  | [ e ] -> e
  | hd :: tl -> if hd < min tl then hd else min tl
