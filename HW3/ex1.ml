let rec rev l = match l with [] -> [] | hd :: tl -> rev tl @ [ hd ]

let rec revrev ll =
  match ll with [ [] ] -> [] | [] -> [] | hd :: tl -> revrev tl @ [ rev hd ]
