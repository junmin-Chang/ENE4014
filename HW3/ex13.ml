type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let rec sigma l u e = if l > u then 0.0 else calc_with_x_value e l +. sigma (l +. 1.0) u e

and integral l u e =
  if l > u then 0.0 else (0.1 *. calc_with_x_value e l) +. integral (l +. 0.1) u e

and calc_with_x_value e v =
  match e with
  | X -> v
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> calc_with_x_value e1 v +. calc_with_x_value e2 v
  | SUB (e1, e2) -> calc_with_x_value e1 v -. calc_with_x_value e2 v
  | MUL (e1, e2) -> calc_with_x_value e1 v *. calc_with_x_value e2 v
  | DIV (e1, e2) -> calc_with_x_value e1 v /. calc_with_x_value e2 v
  | SIGMA (e1, e2, e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    sigma l u e3
  | INTEGRAL (e1, e2, e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    integral l u e3

let rec calculate e =
  match e with
  | X -> raise (Failure "FreeVariable")
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> calculate e1 +. calculate e2
  | SUB (e1, e2) -> calculate e1 -. calculate e2
  | MUL (e1, e2) -> calculate e1 *. calculate e2
  | DIV (e1, e2) -> calculate e1 /. calculate e2
  | SIGMA (e1, e2, e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    sigma l u e3
  | INTEGRAL (e1, e2, e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    integral l u e3
