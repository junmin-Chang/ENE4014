let rec remove x l =
  match l with
  | [] -> []
  | hd :: tl -> if x = hd then remove x tl else hd :: remove x tl

let rec deduplicate lst =
  match lst with [] -> [] | hd :: tl -> hd :: deduplicate (remove hd tl)
