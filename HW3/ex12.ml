type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff exp var =
 let rec aux acc alist = match alist with
 |[] -> acc
  |hd::tl -> aux (acc @ diff hd) tl
in
  match exp with
| VAR str -> VAR str
| POWER (str, n) -> if n = 2 then TIMES [CONST 2; VAR str] else TIMES [CONST n; POWER (str, n-1)]
| TIMES alist -> aux [] alist
