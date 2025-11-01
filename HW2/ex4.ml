exception LengthError

let rec cartesian l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | [], _ -> raise LengthError
  | _, [] -> raise LengthError
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: cartesian t1 t2
