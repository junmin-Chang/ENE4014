let rec npower x n =
match n with
|0 -> 1.0
|_ -> (1.0 /. float_of_int x) *. (npower x (n-1))
