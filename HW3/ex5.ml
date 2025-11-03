type circuit =
  | IN
  | AND of circuit * circuit
  | OR of circuit * circuit

let test_circuit =
  AND (AND (OR (AND (OR (AND (IN, IN), IN), IN), OR (AND (IN, IN), IN)), IN), AND (IN, IN))

let rec and_depth c =
  let max x y = if x > y then x else y in
  match c with
  | IN -> 0
  | OR (c1, c2) -> max (and_depth c1) (and_depth c2)
  | AND (c1, c2) -> 1 + max (and_depth c1) (and_depth c2)
