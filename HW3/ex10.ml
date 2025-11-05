type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc exp =
  match exp with
  | NUM n -> n
  | PLUS (n1, n2) -> calc n1 + calc n2
  | MINUS (n1, n2) -> calc n1 - calc n2

let rec eval fo =
  match fo with
  | TRUE -> true
  | FALSE -> false
  | NOT f -> if eval f = true then false else true
  | ANDALSO (f1, f2) ->
    (match eval f1, eval f2 with
     | true, true -> true
     | _ -> false)
  | ORELSE (f1, f2) ->
    (match eval f1, eval f2 with
     | false, false -> false
     | _ -> true)
  | IMPLY (f1, f2) ->
    (match eval f1, eval f2 with
     | true, false -> false
     | _ -> true)
  | LESS (e1, e2) -> calc e1 < calc e2
