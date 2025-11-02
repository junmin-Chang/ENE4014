let rec dsort lst =
  let rec largest e l =
    match l with
    | [] -> e
    | hd :: tl -> if e >= hd then largest e tl else largest hd tl
  in
  let rec remove x l =
    match l with
    | [] -> []
    | hd :: tl -> if x = hd then tl else hd :: remove x tl
  in
  match lst with
  | [] -> []
  | hd :: tl -> largest hd lst :: dsort (remove (largest hd lst) lst)
