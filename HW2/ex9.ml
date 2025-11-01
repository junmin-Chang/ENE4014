let rec replicate lst n =
  match lst with
  | [] -> []
  | hd :: tl -> (
      if n = 0 then []
      else
        match tl with
        | [] -> hd :: replicate [ hd ] (n - 1)
        | _ -> replicate [ hd ] n @ replicate tl n)
