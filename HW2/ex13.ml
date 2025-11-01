let rec powerset l =
  match l with
  | [] -> [ [] ]
  | hd :: tl ->
      let powerset_tl = powerset tl in
      List.map (fun s -> hd :: s) powerset_tl @ powerset_tl
